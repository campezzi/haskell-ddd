module Account where

import Data.DateTime

type Amount = Double

class Account a where
  {-# MINIMAL (name, balance), (isOpen | isClosed) #-}
  name :: a -> String
  balance :: a -> Amount
  isOpen :: a -> Bool
  isClosed :: a -> Bool
  isOpen = not . isClosed
  isClosed = not . isOpen

data CheckingAccount = CheckingAccount
  { cName :: String
  , cBalance :: Amount
  , cDateOpen :: DateTime
  , cDateClosed :: Maybe DateTime
  }

instance Account CheckingAccount where
  name = cName
  balance = cBalance
  isOpen account
    | cDateClosed account == Nothing = True
    | otherwise = False
