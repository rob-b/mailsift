{-# LANGUAGE OverloadedStrings #-}

module Upload where

import           Control.Lens
import           Control.Monad           (void)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Trans.AWS (runAWST, send)
import           Data.Text               (Text)
import qualified Data.Text.IO            as Text
import           Network.AWS             (Credentials (Discover),
                                          LogLevel (Debug), chunkedFile,
                                          envLogger, envRegion, newEnv,
                                          newLogger, runResourceT)
import           Network.AWS.Data        (toText)
import           Network.AWS.S3
import           System.IO


example :: IO ()
example = do
    -- A new Logger to replace the default noop logger is created, with the logger
    -- set to print debug information and errors to stdout:
    lgr  <- newLogger Debug stdout

    -- To specify configuration preferences, newEnv is used to create a new
    -- configuration environment. The Credentials parameter is used to specify
    -- mechanism for supplying or retrieving AuthN/AuthZ information.
    -- In this case Discover will cause the library to try a number of options such
    -- as default environment variables, or an instance's IAM Profile and identity document:
    env <- newEnv Discover <&> set envLogger lgr . set envRegion Ireland

    -- The payload (and hash) for the S3 object is retrieved from a FilePath:
    body <- chunkedFile 128 "local/path/to/object-payload"

    -- We now run the AWS computation with the overriden logger, performing the
    -- PutObject request. envRegion or within can be used to set the
    -- remote AWS Region:
    runResourceT . runAWST env $ do
        void . send $ putObject "bucket-name" "key" body
        say "Successfully Uploaded: "


say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
