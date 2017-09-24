{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Server where


import           Config                      (Environment (Development),
                                              lookupSetting, setLogger)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (LoggingT, runStdoutLoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Database.Persist.Sql        (ConnectionPool, insert)
import           Database.Persist.Sqlite     (SqlBackend, SqlPersistT,
                                              createSqlitePool, runMigration,
                                              runSqlConn, runSqlPool)
import           Entities                    (Mail(Mail), migrateAll)
import           Network.HTTP.Types.Status   (status422)

import           Network.Wai.Parse           (lbsBackEnd, parseRequestBody)
import           Web.Spock                   (HasSpock, SpockAction, SpockConn,
                                              SpockM, get, json, middleware,
                                              post, request, root, runQuery,
                                              runSpock, setStatus, spock, text)
import           Web.Spock.Config            (PoolOrConn (PCPool),
                                              defaultSpockCfg)

import           Data.Aeson                  (KeyValue, Value (Object), (.=))
import           GHC.Exts                    (fromList)

import qualified Data.ByteString             as B
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8)
import Text.Digestive (Form, check, monadic)
import           Text.Digestive.Aeson        (digestJSON, jsonErrors)
import qualified Text.Digestive.Form         as D
import Data.Time.Clock (getCurrentTime)

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a


emailForm :: (MonadIO m) => Form Text m Mail
emailForm =
  Mail
    <$> "from" D..: nonEmptyText
    <*> "to" D..: nonEmptyText
    <*> "subject" D..: nonEmptyText
    <*> "body" D..: nonEmptyText
    <*> monadic (pure <$> liftIO getCurrentTime)
  where
    nonEmptyText = check "Cannot be empty." (not . T.null) (D.text Nothing)


run :: IO ()
run = do
  env <- lookupSetting "ENV" Development
  port <- lookupSetting "PORT" 8080
  let logger = setLogger env
  pool <- mkPool
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  migrate pool
  runSpock port (spock spockCfg $ middleware logger >> app)


mkPool :: (MonadIO m, MonadBaseControl IO m) => m ConnectionPool
mkPool = runStdoutLoggingT $ createSqlitePool "api.db" 5


migrate :: (MonadBaseControl IO m, MonadIO m) => ConnectionPool -> m ()
migrate pool = runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool


app :: Api
app = do
  get root $ text "hiya"
  post "parse" parseEmailHook


parseEmailHook :: ApiAction a
parseEmailHook = do
    req <- request
    (params, _) <- liftIO $ parseRequestBody lbsBackEnd req
    let dynJson = Object . fromList $ map paramToKeyValue params
    r <- digestJSON emailForm dynJson
    case r of
      (view, Nothing) -> do
        setStatus status422
        json (Object $ fromList ["error" .= jsonErrors view])
      (_, Just email) -> do
        _ <- runSQL $ insert email
        json email


paramToKeyValue :: (KeyValue kv) => (B.ByteString, B.ByteString) -> kv
paramToKeyValue (key, value) = decodeUtf8 key .= decodeUtf8 value


runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn
