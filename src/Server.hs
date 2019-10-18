{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server where



import           Codec.Text.IConv            (reportConversionError)
import           Config                      (AppState, appStateAWSEnv, appStateQueue,
                                              appStateToken, confAppLogger, confAppState, confPool,
                                              confPort, getConfig)
import           Control.Monad.Except        (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (LoggingT, logErrorN, runStdoutLoggingT)
import           Data.Aeson                  (ToJSON, Value (Object, String), decodeStrict, object,
                                              (.=))
import qualified Data.ByteString             as B
import           Data.HVect                  (HVect ((:&:), HNil), ListContains)
import           Data.Maybe                  (fromMaybe, mapMaybe)
import           Data.Monoid                 ((<>))
import           Data.String                 (IsString)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Time.Clock             (getCurrentTime)
import qualified Data.Vector                 as V
import           Database.Persist            (Filter (Filter),
                                              PersistFilter (BackendSpecificFilter),
                                              SelectOpt (Desc, LimitTo), selectList)
import           Database.Persist.Postgresql (SqlBackend)
import           Database.Persist.Sql        (insert)
import           Encoding                    (paramsConversion)
import           Entities                    (Mail (Mail), run2, runSQL, runSQLAction)
import qualified Entities                    as E
import           GHC.Exts                    (fromList)
import           Migration                   (doMigration)
import           Network.HTTP.Types          (Status (Status))
import           Network.HTTP.Types.Status   (status201, status401, status422)
import           Network.Wai.Parse           (defaultParseRequestBodyOptions, lbsBackEnd,
                                              parseRequestBodyEx)
import qualified Queue
import           Types                       (Charset, defaultCharset)
import           Upload                      (uploadFiles)
import qualified Validation                  as Validation
import           Web.Spock                   (ActionCtxT, SpockActionCtx, SpockM, get, getContext,
                                              getSpockPool, getState, header, json, middleware,
                                              paramsGet, post, prehook, request, root, runSpock,
                                              setStatus, spock)
import           Web.Spock.Config            (PoolOrConn (PCPool), SpockCfg, defaultSpockCfg,
                                              spc_errorHandler)


type Api = SpockM SqlBackend () AppState ()
type ApiAction a = SpockActionCtx (HVect '[]) SqlBackend () AppState a
type AuthedApiAction ctx a = SpockActionCtx ctx SqlBackend () AppState a


errorHandler :: Status -> ActionCtxT () IO ()
errorHandler status = json $ prepareError status
  where
    prepareError :: Status -> Value
    prepareError (Status code msg) =
      let inner = V.singleton $ object ["status" .= code, "detail" .= decodeUtf8 msg]
      in object ["errors" .= inner]


-- | Query helpers
selectFilter :: (IsString a, Eq a) => [(a, Text)] -> [Filter Mail]
selectFilter = mapMaybe checkF


checkF :: (Eq a, IsString a) => (a, Text) -> Maybe (Filter Mail)
checkF (key, value)
  | key == "subject" = Just (like E.MailSubject value)
  | key == "from" = Just (like E.MailFromAddress value)
  | key == "to" = Just (like E.MailToAddress value)
  | otherwise = Nothing


like :: E.EntityField record Text -> Text -> Filter record
like field val = Filter field (Left $ T.concat ["%", val, "%"]) (BackendSpecificFilter "ilike")


-- | Routing
app :: Api
app =
  prehook initHook $ do
    post "parse" parseEmailHook
    prehook authHook $ get root emailList


data User = User


initHook :: AuthedApiAction () (HVect '[])
initHook = return HNil


authHook :: AuthedApiAction (HVect xs) (HVect (User ': xs))
authHook = do
  oldCtx <- getContext
  appState <- getState
  auth <- header "Authorization"
  if authCheck auth (appStateToken appState)
    then return (User :&: oldCtx)
    else do
      setStatus status401
      let ej = "sorry. no" :: Value
      json (Object $ fromList ["error" .= ej])


authCheck :: Maybe Text -> String -> Bool
authCheck Nothing _  = False
authCheck (Just x) y = x == "Bearer " <> T.pack y


-- | Handlers
emailList :: ListContains n User xs => AuthedApiAction (HVect xs) a
emailList = do
  params <- paramsGet
  res <- runSQL $ selectList (selectFilter params) [Desc E.MailId, LimitTo 20]
  json $ dataWrapper res


parseEmailHook :: ApiAction a
parseEmailHook = do
  req <- request
  (params, filesMap) <- liftIO $ parseRequestBodyEx defaultParseRequestBodyOptions lbsBackEnd req
  res <- liftIO . runStdoutLoggingT . runExceptT $ validateParams params
  case res of
    Left (ErrorJson ej) -> do
      setStatus status422
      json (Object $ fromList ["error" .= ej])
    Right email -> do
      appState <- getState
      pool <- getSpockPool
      emailKey <- runSQLAction pool (\conn -> run2 conn $ insert email)
      _ <- liftIO $ uploadFiles pool (appStateQueue appState) (appStateAWSEnv appState) emailKey filesMap
      setStatus status201
      json email


-- | Json helpers
dataWrapper :: ToJSON v => v -> Value
dataWrapper a = object ["data" .= a]


newtype ErrorJson = ErrorJson Value
-- data ErrorJson = ErrorJson Value


loadSampleCharset :: IO (Maybe Charset)
loadSampleCharset = decodeStrict <$> B.readFile "charsets.json"


-- | Select the charset as specified by the request parameters
mkCharsetFromParams :: [(B.ByteString, B.ByteString)] -> Maybe Charset
mkCharsetFromParams params = decodeStrict =<< lookup "charsets" params


-- | validate POST params and convert to Mail instance if possible
validateParams :: [(B.ByteString, B.ByteString)] -> ExceptT ErrorJson (LoggingT IO) Mail
validateParams params = do
  let charset = fromMaybe defaultCharset $ mkCharsetFromParams params
  let encodedParams = sequence $ paramsConversion charset params
  case encodedParams of
    Left conversionError -> do
      let failure = show . reportConversionError $ conversionError
      let charsets = filter (\(a, _) -> a == "charsets") params
      logErrorN . T.pack . show $ charsets
      let headers = filter (\(a, _) -> a == "headers") params
      logErrorN . T.pack . show $ headers
      logErrorN $ "Cannot parse POST params: " <> T.pack failure
      throwError $ ErrorJson $ String $ T.pack failure
    Right params' -> do
      case Validation.validateParams params' of
        Left errs -> do
          logErrorN . T.pack . show $ errs
          throwError . ErrorJson $ Validation.errorInfoToValue errs
        Right _ -> do
          (Just m) <- liftIO $ mailFromParams params'
          pure m


mailFromParams :: (MonadIO m) => [(Text, Text)] -> m (Maybe Mail)
mailFromParams params = do
  now <- liftIO getCurrentTime
  let m =
        Mail <$> lookup "fromAddress" params <*> lookup "toAddress" params <*>
        lookup "subject" params <*>
        lookup "body" params <*> Just now
  pure m


-- | Modify the default spock config by setting our custom error handler
mailsiftConfig :: SpockCfg conn sess st -> SpockCfg conn sess st
mailsiftConfig cfg = cfg { spc_errorHandler = errorHandler }


-- | Load the config and start the application
run :: IO ()
run = do
  conf <- getConfig
  let pool = confPool conf
  let port = confPort conf
  let logger = confAppLogger conf
  let appState = confAppState conf
  let queue = appStateQueue appState

  _ <- Queue.worker queue
  spockCfg <- mailsiftConfig <$> defaultSpockCfg () (PCPool pool) appState
  _ <- doMigration
  runSpock port (spock spockCfg $ middleware logger >> app)
