{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Migration where

import           Config                              (confPool, getConfig)
import           Data.Int                            (Int64)
import           Data.Maybe                          (mapMaybe)
import           Data.Text                           (Text)
import           Database.Persist.Migration          (Column (Column),
                                                      ColumnProp (AutoIncrement, NotNull),
                                                      MigrateSql (MigrateSql), Migration,
                                                      MigrationPath ((:=)),
                                                      Operation (AddColumn, CreateTable, DropColumn, RawOperation),
                                                      PersistValue (PersistInt64, PersistText),
                                                      SqlType (SqlDayTime, SqlInt32, SqlString),
                                                      TableConstraint (PrimaryKey), constraints,
                                                      defaultSettings, name, schema, (~>))
import           Database.Persist.Migration.Postgres (runMigration)
import           Database.Persist.Sql                (Single (Single), rawSql, runSqlPool)


createMail :: Operation
createMail = CreateTable
 { name = "mail"
  , schema =
      [ Column "id" SqlInt32 [NotNull, AutoIncrement]
      , Column "from" SqlString [NotNull]
      , Column "to" SqlString [NotNull]
      , Column "subject" SqlString [NotNull]
      , Column "body" SqlString [NotNull]
      , Column "created" SqlDayTime [NotNull]
      ]
  , constraints =
      [ PrimaryKey ["id"]
      ]
  }


migrateFromAddress :: Operation
migrateFromAddress =
  RawOperation "Change field name" $ mapMaybe setFromAddress <$> rawSql "Select id, \"from\" from mail" []


setFromAddress :: (Single Int64, Single (Maybe Text)) -> Maybe MigrateSql
setFromAddress =
  \case
    (_, Single Nothing) -> Nothing
    (Single id', Single (Just address)) ->
      Just $
      MigrateSql
        "UPDATE mail SET from_address = ? WHERE id = ?"
        [PersistText address, PersistInt64 id']


setToAddress :: (Single Int64, Single (Maybe Text)) -> Maybe MigrateSql
setToAddress =
  \case
    (_, Single Nothing) -> Nothing
    (Single id', Single (Just address)) ->
      Just $
      MigrateSql
        "UPDATE mail SET to_address = ? WHERE id = ?"
        [PersistText address, PersistInt64 id']


migrateToAddress :: Operation
migrateToAddress =
  RawOperation "Change column name `to` to `to_address`" $
  mapMaybe setToAddress <$> rawSql "Select id, \"to\" from mail" []


migration :: Migration
migration =
  [ 0 ~> 1 := [createMail]
  , 1 ~> 2 :=
    [ AddColumn
        "mail"
        (Column "from_address" SqlString [NotNull])
        (Just $ PersistText "placeholder")
    , migrateFromAddress
    , DropColumn ("mail", "from")
    ]
  , 2 ~> 3 := [ AddColumn
        "mail"
        (Column "to_address" SqlString [NotNull])
        (Just $ PersistText "placeholder")
    , migrateToAddress
    , DropColumn ("mail", "to")]
  ]


doMigration :: IO ()
doMigration = do
  conf <- getConfig
  let pool = confPool conf
  runSqlPool (runMigration defaultSettings migration) pool
