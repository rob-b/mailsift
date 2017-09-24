module Config where

import Text.Read (readMaybe)
import System.Environment (lookupEnv)
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger
       (logStdout, logStdoutDev)

newtype Config = Config { getEnv :: Environment }


data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)


setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout


lookupSetting
  :: Read a
  => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing -> return def
    Just str -> maybe (handleFailedRead str) return (readMaybe str)
  where
    handleFailedRead str =
      error $ mconcat ["Failed to read [[", str, "]] for environment variable ", env]
