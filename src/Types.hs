{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson   (FromJSON, parseJSON, withObject, (.:))
import           GHC.Generics (Generic)


defaultCharset :: Charset
defaultCharset = Charset {toCharset = "UTF-8", htmlCharset = "iso-8859-1", subjectCharset = "UTF-8", fromCharset = "UTF-8", textCharset = "iso-8859-1"}


data Charset = Charset
  { toCharset      :: String
  , htmlCharset    :: String
  , subjectCharset :: String
  , fromCharset    :: String
  , textCharset    :: String
  } deriving (Show, Generic)


instance FromJSON Charset where
  parseJSON =
    withObject "charset" $ \v ->
      Charset <$> v .: "to" <*> v .: "html" <*> v .: "subject" <*> v .: "from" <*> v .: "text"
