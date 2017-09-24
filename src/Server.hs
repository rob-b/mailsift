{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Server where


import           Config                    (Environment (Development),
                                            lookupSetting, setLogger)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Logger      (LoggingT, runStdoutLoggingT)
import           Database.Persist.Sqlite   (SqlBackend, SqlPersistT,
                                            createSqlitePool, runSqlConn)
import           Entities                  ()
import           Network.HTTP.Types.Status (status422)

import           Network.Wai.Parse         (lbsBackEnd, parseRequestBody)
import           Web.Spock                 (HasSpock, SpockAction, SpockConn,
                                            SpockM, get, json, middleware, post,
                                            request, root, runQuery, runSpock,
                                            setStatus, spock, text)
import           Web.Spock.Config          (PoolOrConn (PCPool),
                                            defaultSpockCfg)

import           Data.Aeson                (FromJSON, KeyValue, ToJSON,
                                            Value (Object), parseJSON, toJSON,
                                            (.:), (.=))
import           Data.Aeson.Types          (camelTo2, defaultOptions,
                                            fieldLabelModifier, genericToJSON,
                                            withObject)

import           GHC.Exts                  (fromList)

import qualified Data.ByteString           as B
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8)
import           GHC.Generics              (Generic)
import           Text.Digestive            (Form, check)
import           Text.Digestive.Aeson      (digestJSON, jsonErrors)
import qualified Text.Digestive.Form       as D

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a


emailForm :: Monad m => Form Text m Email
emailForm =
  Email <$> "to" D..: nonEmptyText <*> "from" D..: nonEmptyText <*> "subject" D..: nonEmptyText

  where nonEmptyText = check "Cannot be empty." (not . T.null) (D.text Nothing)


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
    r <- digestJSON emailForm dynJson
    case r of
      (view, Nothing) -> do
        setStatus status422
        json (Object $ fromList ["errors" .= jsonErrors view])
      (_, Just email) -> json email


expand :: (KeyValue kv) => (B.ByteString, B.ByteString) -> kv
expand (key, value) = decodeUtf8 key .= decodeUtf8 value


runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn