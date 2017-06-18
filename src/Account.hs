{-# LANGUAGE TemplateHaskell #-}

module Account where

import Data.DateTime
import Data.Label

type Amount = Double

data Bank = Bank
  { _name :: String
  } deriving (Show)

data Address = Address
  { _streetName :: String
  , _houseNumber :: Int
  } deriving (Show)

data Account = Account
  { _accountNumber :: String
  , _accountName :: String
  , _bank :: Bank
  , _address :: Address
  , _dateOpen :: DateTime
  , _dateClosed :: Maybe DateTime
  , _balance :: Amount
  } deriving (Show)

data AccountError
  = NotEnoughFunds
  | AccountClosed
  deriving (Show)

mkLabels [''Bank, ''Address, ''Account]

isClosed :: Account -> Bool
isClosed account
  | get dateClosed account == Nothing = False
  | otherwise = True

debit :: Account -> Amount -> Either AccountError Account
debit account amount
  | isClosed account = Left AccountClosed
  | currentBalance < amount = Left NotEnoughFunds
  | otherwise = Right $ set balance (currentBalance - amount) account
  where
    currentBalance = get balance account

credit :: Account -> Amount -> Either AccountError Account
credit account amount
  | isClosed account = Left AccountClosed
  | otherwise = Right $ set balance (currentBalance + amount) account
  where
    currentBalance = get balance account

transfer :: Account -> Amount -> Account -> Either AccountError Account
transfer source amount destination = do
  updatedSource <- debit source amount
  _ <- credit destination amount
  return updatedSource

close :: Account -> Account
close = set dateClosed (Just today)

-- test data
nab :: Bank
nab = Bank "NAB"

home :: Address
home = Address "St. Kilda Road" 213

today :: DateTime
today = fromGregorian 2017 6 18 20 00 00

rich :: Account
rich = Account "r123" "Thiago" nab home today Nothing 10000

poor :: Account
poor = set balance 0 rich
