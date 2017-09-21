{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Mailsift.Server where

import Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist.Sqlite (SqlBackend, SqlPersistT,
                                          createSqlitePool, runSqlConn)
import           Mailsift.Config         (Environment (Development),
                                          lookupSetting, setLogger)
import           Mailsift.Entities       ()
import           Network.Wai             (Request)

import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import           Web.Spock               (HasSpock, SpockAction, SpockConn,
                                          SpockM, body, get, json, jsonBody', middleware,
                                          post, request, root, runQuery,
                                          runSpock, spock, text)
import           Web.Spock.Config        (PoolOrConn (PCPool), defaultSpockCfg)

import           Data.Aeson              (toJSON, Value (Object), Value, (.=))

import           GHC.Exts                (fromList)

import qualified Data.ByteString         as B
import           Data.Text.Encoding      (decodeUtf8)
import Data.Text (Text)

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a


run :: IO ()
run = do
  env <- lookupSetting "ENV" Development
  port <- lookupSetting "PORT" 8080
  let logger = setLogger env
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runSpock port (spock spockCfg $ middleware logger >> app)

app :: Api
app = do
  get root $ do
    text "hiya"
  post "parse" $ do
    req <- request
    (params, _) <- liftIO $ parseRequestBody lbsBackEnd req
    json . Object . fromList $ (map (\(a, b) -> (decodeUtf8 a .= decodeUtf8 b)) params)


runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn
