{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Encoding where

import           Codec.Text.IConv     (ConversionError, convertStrictly)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.String          (IsString)
import           Data.Text            (Text)
import           Data.Text.Encoding   (decodeUtf8)
import           Types                (Charset, fromCharset, htmlCharset, subjectCharset,
                                       textCharset, toCharset)


new :: ByteString -> Text
new key
  | key == "from" = "fromAddress"
  | key == "to" = "toAddress"
  | otherwise = decodeUtf8 key


paramsConversion :: Charset -> [(ByteString, ByteString)] -> [Either ConversionError (Text, Text)]
paramsConversion charset params = fmap finalise (decodeParams charset params)
  where
    finalise :: Either a (ByteString, ByteString) -> Either a (Text, Text)
    finalise param =
      case param of
        Left e             -> Left e
        Right (key, value) -> Right (new key, new value)


decodeParams
  :: (IsString a, Eq a)
  => Charset
  -> [(a, ByteString)]
  -> [Either ConversionError (a, ByteString)]
decodeParams charset params = for params $ \param -> uncurry flippedDecode param
  where
    for = flip map
    doConversion key value fromEncoding =
      case convertStrictly fromEncoding "UTF-8" (BL.fromStrict value) of
        Left b                -> Left (key, BL.toStrict b)
        Right conversionError -> Right conversionError

    flippedDecode :: (IsString a, Eq a) => a -> ByteString -> Either ConversionError (a, ByteString)
    flippedDecode key value = eitherFlip $ decode key value

    decode :: (IsString a, Eq a) => a -> ByteString -> Either (a, ByteString) ConversionError
    decode key value
      | key == "to" = doConversion key value (toCharset charset)
      | key == "html" = doConversion key value (htmlCharset charset)
      | key == "subject" = doConversion key value (subjectCharset charset)
      | key == "from" = doConversion key value (fromCharset charset)
      | key == "text" = doConversion key value (textCharset charset)
      | otherwise = Left (key, value)

    eitherFlip :: Either a b -> Either b a
    eitherFlip = either Right Left
