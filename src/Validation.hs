{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Validation where

import           Control.Monad.Validate (MonadValidate, dispute, runValidate)
import           Data.Aeson             (ToJSON, Value, object, toJSON, (.=))
import           Data.Bool              (bool)
import           Data.Maybe             (isJust)
import           Data.String            (IsString)
import           Data.Text              (Text)
import qualified Data.Text              as T


data ErrorInfo
  = InvalidEmail Text
  | SilentInvalidEmail Text
  | InvalidParams Text
  | MissingParam Text
  | JsonBadValue Text Value
  deriving (Show)


instance ToJSON ErrorInfo  where
  toJSON (JsonBadValue reason val)   = object ["reason" .= reason, "value" .= val]
  toJSON (InvalidEmail reason)       = object ["reason" .= reason]
  toJSON (SilentInvalidEmail reason) = object ["reason" .= reason]
  toJSON (MissingParam reason)       = object ["reason" .= reason]
  toJSON (InvalidParams reason)      = object ["reason" .= reason]


note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right


test :: [(Text, Text)]
test = [("from", "alice@example.com"), ("to", "bobby@example.com")]


mergeValidationResults :: Either [ErrorInfo] () -> Either [ErrorInfo] () -> Either [ErrorInfo] ()
mergeValidationResults (Left xs) (Left ys)   = Left (xs <> ys)
mergeValidationResults (Left xs) (Right ())  = Left xs
mergeValidationResults (Right ()) (Left xs)  = Left xs
mergeValidationResults (Right ()) (Right ()) = Right ()


validateParams :: (Eq a, Data.String.IsString a) => [(a, Text)] -> Either [ErrorInfo] ()
validateParams params = do
  fromAddress <- note [MissingParam "fromAddress"] $ lookup "fromAddress" params
  toAddress <- note [MissingParam "toAddress"] $ lookup "toAddress" params
  let x = emailValidator fromAddress
  let y = emailValidator toAddress
  mergeValidationResults x y


emailValidator :: Text -> Either [ErrorInfo] ()
emailValidator s =
  runValidate $ validateIsNotTestEmail s >> validateIsEmail s >> validateIsNotZDEmail s


disputeIfFalse :: MonadValidate [a] m => a -> Bool -> m ()
disputeIfFalse reason = bool (dispute [reason]) (pure ())


disputeIfTrue :: MonadValidate [a] m => a -> Bool -> m ()
disputeIfTrue reason cond = disputeIfFalse reason (not cond)


validateIsEmail :: (MonadValidate [ErrorInfo] m) => Text -> m ()
validateIsEmail s = disputeIfFalse (InvalidEmail "Email must contain an '@' sign.") (dumbIsEmail s)


validateIsNotZDEmail :: (MonadValidate [ErrorInfo] m) => Text -> m ()
validateIsNotZDEmail s =
  disputeIfTrue (SilentInvalidEmail "We don't store zd emails.") (isZdEmail s)


validateIsNotTestEmail :: (MonadValidate [ErrorInfo] f) => Text -> f ()
validateIsNotTestEmail s =
  disputeIfTrue (SilentInvalidEmail "This is a testing email.") (dumbIsZdEmail s)


-- predicates
-------------------------------------------------------------------------------
dumbIsZdEmail :: Text -> Bool
dumbIsZdEmail = (== "ZD Testing<testing+accounting@zerodeposit.com>")


dumbIsEmail :: Text -> Bool
dumbIsEmail t = isJust $ T.find (=='@') t


isZdEmail :: Text -> Bool
isZdEmail email =
  let indexM = T.findIndex (== '@') email
      pair = flip T.splitAt email <$> indexM
      sndMatches (_, b) = b == "@zerodeposit.com"
  in maybe False sndMatches pair
