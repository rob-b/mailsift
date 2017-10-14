{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Server where

import           Config                      (AppState, appStateAWSEnv, appStateToken,
                                              confAppLogger, confAppState, confPool, confPort,
                                              getConfig)
import           Control.Concurrent          (forkIO)
import           Control.Monad               (forM_, void)
import           Control.Monad.Except        (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (LoggingT, logErrorN, logInfoN, runStdoutLoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson                  (KeyValue, ToJSON, Value (Object), object, (.=))
import qualified Data.ByteString             as B
import           Data.ByteString.Lazy        (ByteString)
import           Data.HVect                  (HVect (HNil, (:&:)), ListContains)
import           Data.Maybe                  (isJust, mapMaybe)
import           Data.Monoid                 ((<>))
import           Data.Pool                   (Pool, withResource)
import           Data.String                 (IsString)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Time.Clock             (getCurrentTime)
import qualified Data.Vector                 as V
import           Database.Persist            (Filter (Filter), IsPersistBackend, Key,
                                              PersistFilter (BackendSpecificFilter),
                                              SelectOpt (Desc), selectList)
import           Database.Persist.Postgresql (BaseBackend, PersistStoreWrite, SqlBackend,
                                              fromSqlKey, runMigration, runSqlPool)
import           Database.Persist.Sql        (ConnectionPool, insert)
import           Entities                    (Mail (Mail), migrateAll, run2, runSQL)
import qualified Entities                    as E
import           GHC.Exts                    (fromList)
import           Network.AWS.Env             (Env)
import           Network.AWS.S3              (ObjectKey (ObjectKey))
import           Network.HTTP.Types          (Status (Status))
import           Network.HTTP.Types.Status   (status201, status401, status422)
import           Network.Wai.Parse           (FileInfo, defaultParseRequestBodyOptions,
                                              fileContent, fileName, lbsBackEnd,
                                              parseRequestBodyEx)
import           Text.Digestive              (Form, check, monadic)
import           Text.Digestive.Aeson        (digestJSON, jsonErrors)
import qualified Text.Digestive.Form         as D
import           Upload                      (uploadAndSave)
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
    <$> "from" D..: check "Not a valid email address." checkEmail (D.text Nothing)
    <*> "to" D..: check "Not a valid email address." checkEmail (D.text Nothing)
    <*> "subject" D..: nonEmptyText
    <*> "text" D..: D.text Nothing
    <*> monadic (pure <$> liftIO getCurrentTime)
  where
    nonEmptyText = check "Cannot be empty." (not . T.null) (D.text Nothing)
    checkEmail :: Text -> Bool
    checkEmail = isJust . T.find (== '@')


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
authCheck Nothing _ = False
authCheck (Just x) y = x == "Bearer " <> T.pack y


-- | Handlers
emailList :: ListContains n User xs => AuthedApiAction (HVect xs) a
emailList = do
  params <- paramsGet
  res <- runSQL $ selectList (selectFilter params) [Desc E.MailId]
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
      _ <- liftIO $ uploadFiles pool (appStateAWSEnv appState) emailKey filesMap
      setStatus status201
      json email


-- | Json helpers
dataWrapper :: ToJSON v => v -> Value
dataWrapper a = object ["data" .= a]


newtype ErrorJson = ErrorJson Value


runSQLAction :: MonadIO m => Pool a1 -> (a1 -> IO a) -> m a
runSQLAction pool query = liftIO $ withResource pool query


uploadFiles
  :: ( BaseBackend backend ~ SqlBackend
     , PersistStoreWrite backend
     , IsPersistBackend backend
     , Foldable t1
     )
  => Pool backend -> Env -> Key Mail -> t1 (t, FileInfo ByteString) -> IO ()
uploadFiles pool env key things =
  forM_ things $ \thing -> do
    _ <- uploadFile pool env key thing
    pure ()


uploadFile
  :: (BaseBackend backend ~ SqlBackend, IsPersistBackend backend, PersistStoreWrite backend)
  => Pool backend -> Env -> Key Mail -> (t, FileInfo ByteString) -> IO ()
uploadFile pool env key (_, fInfo) = do
  let dbAction = uploadAndSave env key (mkname key fInfo) (fileContent fInfo)
  _ <- forkIO $ void (runSQLAction pool (`run2` dbAction))
  pure ()
  where
    mkname key' fInfo' =
      ObjectKey $ T.pack (show (fromSqlKey key')) <> "/" <> decodeUtf8 (fileName fInfo')


validateParams :: [(B.ByteString, B.ByteString)] -> ExceptT ErrorJson (LoggingT IO) Mail
validateParams params = do
  let dynJson = Object . fromList $ map paramToKeyValue params
  r <- digestJSON emailForm dynJson
  logInfoN $ T.pack (show params)
  case r of
    (view, Nothing) -> do
      let err = jsonErrors view
      logErrorN . T.pack . show $ err
      throwError . ErrorJson $ err
    (_, Just email) -> pure email


paramToKeyValue :: (KeyValue kv) => (B.ByteString, B.ByteString) -> kv
paramToKeyValue (key, value) = decodeUtf8 key .= decodeUtf8 value


migrate :: (MonadBaseControl IO m, MonadIO m) => ConnectionPool -> m ()
migrate pool = runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool


-- | Load the config and start the application
run :: IO ()
run = do
  conf <- getConfig
  let pool = confPool conf
  let port = confPort conf
  let logger = confAppLogger conf
  spockCfg <- mailsiftConfig <$> defaultSpockCfg () (PCPool pool) (confAppState conf)
  migrate pool
  runSpock port (spock spockCfg $ middleware logger >> app)


selectFilter :: (IsString a, Eq a) => [(a, Text)] -> [Filter Mail]
selectFilter = mapMaybe checkF


checkF :: (Eq a, IsString a) => (a, Text) -> Maybe (Filter Mail)
checkF (key, value)
  | key == "subject" = Just (like E.MailSubject value)
  | key == "from" = Just (like E.MailFrom value)
  | key == "to" = Just (like E.MailTo value)
  | otherwise = Nothing


like :: E.EntityField record Text -> Text -> Filter record
like field val = Filter field (Left $ T.concat ["%", val, "%"]) (BackendSpecificFilter "ilike")
