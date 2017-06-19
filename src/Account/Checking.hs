module Account.Checking where

import Account.Classes
import Aliases
import Data.DateTime

data CheckingAccount = CheckingAccount
  { cId :: String
  , cName :: String
  , cBalance :: Amount
  , cDateOpen :: DateTime
  , cDateClosed :: Maybe DateTime
  }

instance Show CheckingAccount where
  show (CheckingAccount i n b _ _) = i ++ " - " ++ n ++ ": $" ++ (show b)

instance Account CheckingAccount where
  accountId = cId
  name = cName
  balance = cBalance
  isOpen account
    | cDateClosed account == Nothing = True
    | otherwise = False
  credit account amount
    | isClosed account = Left AccountClosed
    | otherwise = Right $ account {cBalance = (balance account) + amount}
  debit account amount
    | isClosed account = Left AccountClosed
    | balance account < amount = Left InsufficientFunds
    | otherwise = Right $ account {cBalance = (balance account) - amount}
  close account when
    | isClosed account = Left AccountClosed
    | otherwise = Right $ account {cDateClosed = Just when}
