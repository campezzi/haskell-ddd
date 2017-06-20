{-# LANGUAGE TemplateHaskell #-}

module Account.Checking where

import Account.Classes
import Aliases
import Control.Lens
import Data.DateTime

data CheckingAccount = CheckingAccount
  { _cAccountId :: String
  , _cName :: String
  , _cBalance :: Amount
  , _cDateOpen :: DateTime
  , _cDateClosed :: Maybe DateTime
  }

makeLenses ''CheckingAccount

instance Show CheckingAccount where
  show (CheckingAccount i n b _ _) = i ++ " - " ++ n ++ ": $" ++ (show b)

instance Account CheckingAccount where
  accountId = cAccountId
  name = cName
  balance = cBalance
  dateClosed = cDateClosed
