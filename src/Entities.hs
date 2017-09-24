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

import           Data.Text           (Text)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
import Data.Time.Clock (UTCTime)


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
|]
