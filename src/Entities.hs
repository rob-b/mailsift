{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Entities where

import           Control.Monad.Logger        (LoggingT, runStdoutLoggingT)
import           Data.Text                   (Text)
import           Data.Time.Clock             (UTCTime)
import           Database.Persist.Postgresql (SqlBackend, SqlPersistT, runSqlConn)
import           Database.Persist.TH         (mkMigrate, mkPersist, persistLowerCase, share,
                                              sqlSettings)
import           Web.Spock                   (HasSpock, SpockConn, runQuery)


runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn


run2 conn action = runStdoutLoggingT $ runSqlConn action conn


share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Mail json
  from Text
  to Text
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
