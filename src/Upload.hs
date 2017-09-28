{-# LANGUAGE OverloadedStrings #-}

module Upload where

import           Control.Lens            (set, (<&>))
import           Control.Monad           (void)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Trans.AWS (runAWST, send)
import           Data.ByteString.Lazy    (ByteString)
import           Data.Text               (Text)
import           Network.AWS             (Credentials (Discover), envRegion,
                                          newEnv, runResourceT, toBody)
import           Network.AWS.S3          (ObjectKey (ObjectKey),
                                          Region (Ireland), putObject)


upload :: (MonadIO m) => Text -> ByteString -> m ()
upload fname fcontent = do
    env <- liftIO $ newEnv Discover <&> set envRegion Ireland
    liftIO . runResourceT . runAWST env $
        void . send $ putObject "zd-attachments" (ObjectKey fname) (toBody fcontent)
