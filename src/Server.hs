{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server where

import           Config                      (AppState, appStateAWSEnv, appStateQueue,
                                              appStateToken, confAppLogger, confAppState, confPool,
                                              confPort, getConfig)
import           Control.Monad.Except        (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (LoggingT, logErrorN, runStdoutLoggingT)
import           Data.Aeson                  (KeyValue, ToJSON, Value (Object), object, (.=))
import qualified Data.ByteString             as B
import           Data.HVect                  (HVect ((:&:), HNil), ListContains)
import           Data.Maybe                  (isJust, mapMaybe)
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
import           Entities                    (Mail (Mail), run2, runSQL, runSQLAction)
import qualified Entities                    as E
import           GHC.Exts                    (fromList)
import           Migration                   (doMigration)
import           Network.HTTP.Types          (Status (Status))
import           Network.HTTP.Types.Status   (status201, status401, status422)
import           Network.Wai.Parse           (defaultParseRequestBodyOptions, lbsBackEnd,
                                              parseRequestBodyEx)
import qualified Queue
import           Text.Digestive              (Form, check, monadic)
import           Text.Digestive.Aeson        (digestJSON, jsonErrors)
import qualified Text.Digestive.Form         as D
import           Text.Digestive.Types        (Result (Error, Success))
import           Upload                      (uploadFiles)
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


mailsiftConfig :: SpockCfg conn sess st -> SpockCfg conn sess st
mailsiftConfig cfg = cfg { spc_errorHandler = errorHandler }


emailForm :: (MonadIO m) => Form Text m Mail
emailForm =
  Mail
    <$> "fromAddress" D..: validateEmail
    <*> "toAddress" D..: validateEmail
    <*> "subject" D..: nonEmptyText
    <*> "text" D..: D.text Nothing
    <*> monadic (pure <$> liftIO getCurrentTime)
  where
    nonEmptyText = check "Cannot be empty." (not . T.null) (D.text Nothing)


validateEmail :: Monad m => Form Text m Text
validateEmail = D.validate checker2 (D.text Nothing)


checker2 :: Text -> Result Text Text
checker2 t
  | dumbIsZdEmail t = Error (t <> " is a zd testing account.")
  | isZdEmail t = Error (t <> " is not a valid email address.")
  | isJust (T.find (== '@') t) = Success t
  | otherwise = Error (t <> " is not a valid email address.")


isZdEmail :: Text -> Bool
isZdEmail email = combiner $ (\pair -> snd pair == "@zerodeposit.com") . flip T.splitAt email <$> T.findIndex (== '@') email


dumbIsZdEmail :: Text -> Bool
dumbIsZdEmail = (== "ZD Testing<testing+accounting@zerodeposit.com>")


combiner :: Maybe Bool -> Bool
combiner Nothing      = False
combiner (Just False) = False
combiner (Just True)  = True


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


validateParams :: [(B.ByteString, B.ByteString)] -> ExceptT ErrorJson (LoggingT IO) Mail
validateParams params = do
  let dynJson = Object . fromList $ map paramToKeyValue params
  r <- digestJSON emailForm dynJson
  case r of
    (view, Nothing) -> do
      let err = jsonErrors view
      logErrorN . T.pack . show $ err
      throwError . ErrorJson $ err
    (_, Just email) -> pure email


paramToKeyValue :: (KeyValue kv) => (B.ByteString, B.ByteString) -> kv
paramToKeyValue (key, value) = new key .= decodeUtf8 value


new :: B.ByteString -> Text
new key
  | key == "from" = "fromAddress"
  | key == "to" = "toAddress"
  | otherwise = decodeUtf8 key


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
