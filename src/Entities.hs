{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Entities where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (LoggingT, runStdoutLoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson                  (ToJSON, Value (Object), object, toJSON, (.=))
import qualified Data.HashMap.Strict         as HM
import           Data.Pool                   (Pool, withResource)
import           Data.Text                   (Text)
import           Data.Time.Clock             (UTCTime)
import           Database.Persist            (Entity (Entity))
import           Database.Persist.Postgresql (SqlBackend, SqlPersistT, runMigration, runSqlConn,
                                              runSqlPool)
import           Database.Persist.Sql        (ConnectionPool)
import           Database.Persist.TH         (mkMigrate, mkPersist, persistLowerCase, share,
                                              sqlSettings)
import           Web.Spock                   (HasSpock, SpockConn, runQuery)


share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Mail
  fromAddress Text sql=from_address
  toAddress Text sql=to_address
  subject Text
  body Text
  created UTCTime sql=created
  deriving Show

Attachment json
  filename Text
  mimetype Text
  url Text
  mail MailId
  created UTCTime
  deriving Show
|]


instance ToJSON (Entity Mail) where
  toJSON (Entity key value) = case toJSON value of
    Object o -> Object $ HM.insert "id" (toJSON key) o
    x        -> x


instance ToJSON Mail where
  toJSON Mail {..} =
    object
      [
        "body" .= mailBody
      , "created" .= mailCreated
      , "from_address" .= mailFromAddress
      , "subject" .= mailSubject
      , "to_address" .= mailToAddress
      ]


migrate :: (MonadBaseControl IO m, MonadIO m) => ConnectionPool -> m ()
migrate pool = runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool


runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn


run2
  :: (MonadBaseControl IO m, MonadIO m)
  => SqlBackend -> SqlPersistT (LoggingT m) a -> m a
run2 conn action = runStdoutLoggingT $ runSqlConn action conn


runSQLAction :: MonadIO m => Pool a1 -> (a1 -> IO a) -> m a
runSQLAction pool query = liftIO $ withResource pool query
