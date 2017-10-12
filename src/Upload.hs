{-# LANGUAGE OverloadedStrings #-}

module Upload where

import           Control.Lens            ((^.), set, (<&>))
import           Control.Monad           (void)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.AWS (runAWST, send)
import           Data.ByteString.Lazy    (ByteString)
import           Data.Text               (Text)
import           Data.Time (getCurrentTime)
import           Network.AWS (Credentials(Discover), envRegion, newEnv, runResourceT, toBody, envAuth)
import           Network.AWS.Data.Path (rawPath)
import           Network.AWS.Presign (presignURL)
import           Network.AWS.S3 (ObjectKey(ObjectKey), Region(Ireland), putObject, poBucket, getObject, BucketName)
-- import Network.AWS.S3.PutObject (toPath)


zdBucket :: BucketName
zdBucket = "zd-attachments"


upload :: (MonadIO m) => Text -> ByteString -> m ()
upload fname fcontent = do
    env <- liftIO $ newEnv Discover <&> set envRegion Ireland
    liftIO . runResourceT . runAWST env $ do
        let pobj = putObject zdBucket (ObjectKey fname) (toBody fcontent)
        let gobj = getObject zdBucket (ObjectKey fname)
        rightNow <- liftIO getCurrentTime
        signed <- lift $ presignURL (env ^. envAuth) Ireland rightNow 500 gobj
        liftIO $ print signed
        liftIO $ print (pobj ^. poBucket)
        pure ()
        -- void $ traceShow (show pobj) (send pobj)
