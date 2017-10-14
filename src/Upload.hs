{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Upload where

import           Control.Lens                ((^.))
import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (ReaderT)
import           Control.Monad.Trans.AWS     (runAWST, send)
import qualified Data.ByteString             as B
import           Data.ByteString.Lazy        (ByteString)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Time                   (getCurrentTime)
import           Database.Persist            (Key)
import           Database.Persist.Postgresql (PersistRecordBackend, PersistStoreWrite)
import           Database.Persist.Sql        (insert)
import qualified Entities                    as E
import           Network.AWS                 (Auth, Env, envAuth, runResourceT, toBody)
import           Network.AWS.Data.Text       (ToText, toText)
import           Network.AWS.Presign         (presignURL)
import           Network.AWS.S3              (BucketName, ObjectKey, Region (Ireland), getObject,
                                              putObject)



zdBucket :: BucketName
zdBucket = "zd-attachments"


longURL :: (ToText a, ToText b) => a -> b -> Text
longURL bucket key = "https://" <> toText bucket <> ".s3.amazonaws.com/" <> toText key


upload :: (MonadIO m) => Env -> ObjectKey -> ByteString -> m ()
upload env objectKey fcontent =
    liftIO . runResourceT . runAWST env $ do
        let pobj = putObject zdBucket objectKey (toBody fcontent)
        void $ send pobj


signObjectKey :: MonadIO m => Auth -> ObjectKey -> m B.ByteString
signObjectKey auth objectKey = do
  let gobj = getObject zdBucket objectKey
  rightNow <- liftIO getCurrentTime
  presignURL auth Ireland rightNow 300 gobj


uploadAndSave
  :: (PersistStoreWrite backend, PersistRecordBackend E.Attachment backend, MonadIO m)
  => Env -> Key E.Mail -> ObjectKey -> ByteString -> ReaderT backend m (Key E.Attachment)
uploadAndSave env mail objectKey content = do
  _ <- upload env objectKey content
  (signed, _) <- B.breakSubstring "?" <$> signObjectKey (env ^. envAuth) objectKey
  rightNow <- liftIO getCurrentTime
  let attach =
        E.Attachment
        { E.attachmentFilename = toText objectKey
        , E.attachmentMimetype = "application/pdf"
        , E.attachmentUrl = decodeUtf8 signed
        , E.attachmentMail = mail
        , E.attachmentCreated = rightNow
        }
  insert attach
