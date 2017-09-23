{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Mailsift.Server where


import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Logger      (LoggingT, runStdoutLoggingT)
import           Database.Persist.Sqlite   (SqlBackend, SqlPersistT,
                                            createSqlitePool, runSqlConn)
import           Mailsift.Config           (Environment (Development),
                                            lookupSetting, setLogger)
import           Mailsift.Entities         ()
import           Network.HTTP.Types.Status (status422)
import           Network.Wai               (Request)

import           Network.Wai.Parse         (lbsBackEnd, parseRequestBody)
import           Web.Spock                 (HasSpock, SpockAction, SpockConn,
                                            SpockM, body, get, json, jsonBody,
                                            middleware, post, request, root,
                                            runQuery, runSpock, setStatus,
                                            spock, text)
import           Web.Spock.Config          (PoolOrConn (PCPool),
                                            defaultSpockCfg)

import           Data.Aeson                (FromJSON, KeyValue, ToJSON,
                                            Value (Object), Value, eitherDecode, encode,
                                            parseJSON, toJSON, (.:), (.=))
import           Data.Aeson.Types          (camelTo2, defaultOptions,
                                            fieldLabelModifier, genericToJSON,
                                            withObject)

import           GHC.Exts                  (fromList)

import qualified Data.ByteString           as B
import qualified Data.Text as T
import           Data.Text                 (Text)
import           Data.Text.Encoding        (decodeUtf8)
import           GHC.Generics              (Generic)

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a


data Email = Email
  { emailTo :: Text
  , emailFrom :: Text
  , emailSubject :: Text
  } deriving (Show, Generic)


instance FromJSON Email where
  parseJSON = withObject "Email" $ \o -> Email <$> o .: "to" <*> o .: "from" <*> o .: "subject"


instance ToJSON Email where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 6 . camelTo2 '_' }


run :: IO ()
run = do
  env <- lookupSetting "ENV" Development
  port <- lookupSetting "PORT" 8080
  let logger = setLogger env
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runSpock port (spock spockCfg $ middleware logger >> app)


app :: Api
app = do
  get root $ text "hiya"
  post "parse" $ do
    req <- request
    (params, _) <- liftIO $ parseRequestBody lbsBackEnd req
    let dynJson = Object . fromList $ map expand params
    let dynValue = encode dynJson
    let xo = eitherDecode dynValue :: Either String Email
    email <- case xo of
      Left err -> do
        setStatus status422
        json (Object $ fromList ["error" .= T.pack err])
      Right val -> return val
    json email


expand :: (KeyValue kv) => (B.ByteString, B.ByteString) -> kv
expand (key, value) = decodeUtf8 key .= decodeUtf8 value


runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn
