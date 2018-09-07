{-# LANGUAGE OverloadedStrings #-}
module Config where

import           Control.Concurrent.STM.TBQueue       (TBQueue)
import           Control.Lens                         (set, (<&>))
import           Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)
import           Data.Maybe                           (fromMaybe)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (encodeUtf8)
import qualified Database.Persist.Postgresql          as DB
import           Database.Persist.Sql                 (ConnectionPool)
import           Network.AWS                          (Region (Ireland))
import           Network.AWS.Auth                     (Credentials (Discover))
import           Network.AWS.Env                      (Env, envRegion, newEnv)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Queue
import           System.Environment                   (lookupEnv)
import           Text.Read                            (readMaybe)
import           Web.Heroku                           (parseDatabaseUrl)


data AppState = AppState
  { appStateAWSEnv :: Env
  , appStateToken  :: String
  , appStateQueue  :: TBQueue (IO ())
  }


data Config = Config
  { confEnv       :: Environment
  , confPort      :: Int
  , confPool      :: ConnectionPool
  , confAppLogger ::  Middleware
  , confAppState  :: AppState
  }


getAWSEnv :: IO Env
getAWSEnv = newEnv Discover <&> set envRegion Ireland


getConfig :: IO Config
getConfig = do
  awsEnv <- getAWSEnv
  env <- lookupSettingSafe "ENV" Development
  port <- lookupSettingSafe "PORT" 8080
  rdsDbUrl <- lookupEnv "RDS_DATABASE_URL"
  dbUrl' <- lookupSetting "DATABASE_URL" "postgres://mailsift:@localhost:5432/mailsift"
  token <- lookupSetting "AUTH_TOKEN" "it would be better to just break here"
  queue <- Queue.make
  let dbUrl = fromMaybe dbUrl' rdsDbUrl
  let logger = setLogger env
  let s = createConnectionString (parseDatabaseUrl dbUrl)
  let appState = AppState {appStateToken = token, appStateAWSEnv = awsEnv, appStateQueue = queue}
  pool <-
    case env of
      Production  -> runStdoutLoggingT (DB.createPostgresqlPool s 4)
      Development -> runStdoutLoggingT (DB.createPostgresqlPool s 1)
      Test        -> runNoLoggingT (DB.createPostgresqlPool s 1)
  pure
    Config
    { confEnv = env
    , confPort = port
    , confPool = pool
    , confAppLogger = logger
    , confAppState = appState
    }


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
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout


lookupSettingSafe
  :: Read a
  => String -> a -> IO a
lookupSettingSafe env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing  -> return def
    Just str -> maybe (handleFailedRead str) return (readMaybe str)
  where
    handleFailedRead str =
      error $ mconcat ["Failed to read [[", str, "]] for environment variable ", env]


lookupSetting :: String -> String -> IO String
lookupSetting key fallback = fromMaybe fallback <$>lookupEnv key
