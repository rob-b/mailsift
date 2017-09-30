{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Server where

import           Config                      (confAppLogger, confPool, confPort,
                                              getConfig)
import           Control.Concurrent.Async    (mapConcurrently_)
import           Control.Monad.Except        (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (LoggingT, logErrorN, logInfoN,
                                              runStdoutLoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Monoid                 ((<>))
import           Database.Persist            (Key, SelectOpt (Desc), selectList)
import           Database.Persist.Postgresql (SqlBackend, SqlPersistT,
                                              fromSqlKey, runMigration,
                                              runSqlConn, runSqlPool)
import           Database.Persist.Sql        (ConnectionPool, insert)
import           Entities                    (Mail (Mail), migrateAll)
import qualified Entities                    as E
import           Network.HTTP.Types          (Status (Status))
import           Network.HTTP.Types.Status   (status201, status422)

import           Network.Wai.Parse           (FileInfo,
                                              defaultParseRequestBodyOptions,
                                              fileContent, fileName, lbsBackEnd,
                                              parseRequestBodyEx)
import           Web.Spock                   (ActionCtxT, HasSpock, SpockAction,
                                              SpockConn, SpockM, get, json,
                                              middleware, post, request, root,
                                              runQuery, runSpock, setStatus,
                                              spock)
import           Web.Spock.Config            (PoolOrConn (PCPool), SpockCfg,
                                              defaultSpockCfg, spc_errorHandler)

import           Data.Aeson                  (KeyValue, ToJSON, Value (Object),
                                              object, (.=))
import           GHC.Exts                    (fromList)

import qualified Data.ByteString             as B
import           Data.ByteString.Lazy        (ByteString)
import           Data.Maybe                  (isJust)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Time.Clock             (getCurrentTime)
import qualified Data.Vector                 as V
import           Text.Digestive              (Form, check, monadic)
import           Text.Digestive.Aeson        (digestJSON, jsonErrors)
import qualified Text.Digestive.Form         as D
import           Upload                      (upload)


type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a


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
app = do
  get root emailList
  post "parse" parseEmailHook


-- | Handlers
rootResource :: ApiAction a
rootResource =
  let static = "hassle..." :: Text
  in json . dataWrapper $ object ["attributes" .= static]


emailList :: ApiAction a
emailList = do
  res <- runSQL $ selectList [] [Desc E.MailCreated]
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
      emailKey <- runSQL $ insert email
      _ <- liftIO $ uploadFiles emailKey filesMap
      setStatus status201
      json email


-- | Json helpers
dataWrapper :: ToJSON v => v -> Value
dataWrapper a = object ["data" .= a]


newtype ErrorJson = ErrorJson Value


uploadFiles
  :: (Foldable t1)
  => Key Mail -> t1 (t, FileInfo ByteString) -> IO ()
uploadFiles key = mapConcurrently_ (uploadFile key)


uploadFile :: (MonadIO m) => Key Mail -> (t, FileInfo ByteString) -> m ()
uploadFile key (_, fInfo) = upload (mkname key fInfo) (fileContent fInfo)
  where
    mkname key' fInfo' = T.pack (show (fromSqlKey key')) <> "/" <> decodeUtf8 (fileName fInfo')


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


-- | Execute a given sql command
runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn


-- | Load the config and start the application
run :: IO ()
run = do
  conf <- getConfig
  let pool = confPool conf
  let port = confPort conf
  let logger = confAppLogger conf
  spockCfg <- mailsiftConfig <$> defaultSpockCfg () (PCPool pool) ()
  migrate pool
  runSpock port (spock spockCfg $ middleware logger >> app)
