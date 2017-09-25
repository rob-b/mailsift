{-# LANGUAGE OverloadedStrings #-}
module Config where

import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Data.Maybe                           (fromMaybe)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (encodeUtf8)
import qualified Database.Persist.Postgresql          as DB
import           Database.Persist.Sql                 (ConnectionPool)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           System.Environment                   (lookupEnv)
import           Text.Read                            (readMaybe)
import           Web.Heroku                           (parseDatabaseUrl)


data Config = Config
  { confEnv :: Environment
  , confPort :: Int
  , confPool :: ConnectionPool
  , confAppLogger ::  Middleware
  }


getConfig :: IO Config
getConfig = do
  env <- lookupSettingSafe "ENV" Development
  port <- lookupSettingSafe "PORT" 8080
  dbUrl <- lookupSetting "DATABASE_URL" "postgres://mailsift:@localhost:5432/mailsift"
  let logger = setLogger env
  let s = createConnectionString (parseDatabaseUrl dbUrl)
  pool <- case env of
    Production -> runStdoutLoggingT (DB.createPostgresqlPool s 4)
    Development -> runStdoutLoggingT (DB.createPostgresqlPool s 1)
    Test -> runNoLoggingT (DB.createPostgresqlPool s 1)
  pure Config { confEnv = env, confPort = port, confPool = pool, confAppLogger = logger }


createConnectionString :: [(T.Text, T.Text)] -> DB.ConnectionString
createConnectionString l =
  let f (k, v) = T.concat [k, "=", v]
  in encodeUtf8 (T.unwords (map f l))


data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)


setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout


lookupSettingSafe
  :: Read a
  => String -> a -> IO a
lookupSettingSafe env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing -> return def
    Just str -> maybe (handleFailedRead str) return (readMaybe str)
  where
    handleFailedRead str =
      error $ mconcat ["Failed to read [[", str, "]] for environment variable ", env]


lookupSetting :: String -> String -> IO String
lookupSetting key fallback = do
  maybeValue <- lookupEnv key
  pure $ fromMaybe fallback maybeValue
