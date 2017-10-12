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
import           Network.AWS.Presign (presignURL)
import Network.AWS.S3 (ObjectKey(ObjectKey), Region(Ireland), putObject, poBucket, getObject, BucketName, poKey)
import Network.AWS.Data.Text (toText, ToText)
import Data.Monoid ((<>))


zdBucket :: BucketName
zdBucket = "zd-attachments"


longURL :: (ToText a, ToText b) => a -> b -> Text
longURL bucket key = "https://" <> toText bucket <> ".s3.amazonaws.com/" <> toText key


upload :: (MonadIO m) => Text -> ByteString -> m ()
upload fname fcontent = do
    env <- liftIO $ newEnv Discover <&> set envRegion Ireland
    liftIO . runResourceT . runAWST env $ do
        let pobj = putObject zdBucket (ObjectKey fname) (toBody fcontent)
        let gobj = getObject zdBucket (ObjectKey fname)
        rightNow <- liftIO getCurrentTime
        signed <- lift $ presignURL (env ^. envAuth) Ireland rightNow 500 gobj
        liftIO $ print signed
        liftIO $ print (longURL (pobj ^. poBucket) (pobj ^. poKey))
        pure ()
        -- void $ traceShow (show pobj) (send pobj)
