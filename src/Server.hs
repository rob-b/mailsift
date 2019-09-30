{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server where


import           Codec.Text.IConv            (ConversionError, convertStrictly,
                                              reportConversionError)
import           Config                      (AppState, appStateAWSEnv, appStateQueue,
                                              appStateToken, confAppLogger, confAppState, confPool,
                                              confPort, getConfig)
import           Control.Monad.Except        (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (LoggingT, logErrorN, runStdoutLoggingT)
import           Data.Aeson                  (FromJSON, KeyValue, ToJSON, Value (Object, String),
                                              decodeStrict, object, parseJSON, withObject, (.:),
                                              (.=))
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import           Data.HVect                  (HVect ((:&:), HNil), ListContains)
import           Data.Maybe                  (fromMaybe, isJust, mapMaybe)
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
import           GHC.Generics                (Generic)
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


data Charset = Charset
  { toCharset      :: String
  , htmlCharset    :: String
  , subjectCharset :: String
  , fromCharset    :: String
  , textCharset    :: String
  } deriving (Show, Generic)


defaultCharset :: Charset
defaultCharset = Charset {toCharset = "UTF-8", htmlCharset = "iso-8859-1", subjectCharset = "UTF-8", fromCharset = "UTF-8", textCharset = "iso-8859-1"}


instance FromJSON Charset where
  parseJSON =
    withObject "charset" $ \v ->
      Charset <$> v .: "to" <*> v .: "html" <*> v .: "subject" <*> v .: "from" <*> v .: "text"


loadSampleCharset :: IO (Maybe Charset)
loadSampleCharset = decodeStrict <$> B.readFile "charsets.json"


mkCharsetFromParams :: [(B.ByteString, B.ByteString)] -> Maybe Charset
mkCharsetFromParams params = decodeStrict =<< lookup "charsets" params


note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right


finaliseParams
  :: (KeyValue kv)
  => [Either ConversionError (B.ByteString, B.ByteString)] -> [Either ConversionError kv]
finaliseParams params = map finalise params
  where
    finalise
      :: (KeyValue kv)
      => Either a (B.ByteString, B.ByteString) -> Either a kv
    finalise param =
      case param of
        Left e             -> Left e
        Right (key, value) -> Right (new key .= decodeUtf8 value)


new :: B.ByteString -> Text
new key
  | key == "from" = "fromAddress"
  | key == "to" = "toAddress"
  | otherwise = decodeUtf8 key


paramsConversion
  :: (KeyValue kv)
  => Charset -> [(B.ByteString, B.ByteString)] -> [Either ConversionError kv]
paramsConversion charset params = finaliseParams $ decodeParams charset params


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
      let dynJson = Object . fromList $ params'
      r <- digestJSON emailForm dynJson
      case r of
        (view, Nothing) -> do
          let err = jsonErrors view
          logErrorN . T.pack . show $ err
          throwError . ErrorJson $ err
        (_, Just email) -> pure email


decodeParams
  :: (IsString a, Eq a)
  => Charset
  -> [(a, B.ByteString)]
  -> [Either ConversionError (a, B.ByteString)]
decodeParams charset params = for params $ \param -> uncurry flippedDecode param
  where
    for = flip map
    doConversion key value fromEncoding =
      case convertStrictly fromEncoding "UTF-8" (BL.fromStrict value) of
        Left b                -> Left (key, BL.toStrict b)
        Right conversionError -> Right conversionError

    flippedDecode :: (IsString a, Eq a) => a -> B.ByteString -> Either ConversionError (a, B.ByteString)
    flippedDecode key value = eitherFlip $ decode key value

    decode :: (IsString a, Eq a) => a -> B.ByteString -> Either (a, B.ByteString) ConversionError
    decode key value
      | key == "to" = doConversion key value (toCharset charset)
      | key == "html" = doConversion key value (htmlCharset charset)
      | key == "subject" = doConversion key value (subjectCharset charset)
      | key == "from" = doConversion key value (fromCharset charset)
      | key == "text" = doConversion key value (textCharset charset)
      | otherwise = Left (key, value)


eitherFlip :: Either a b -> Either b a
eitherFlip = either Right Left


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
