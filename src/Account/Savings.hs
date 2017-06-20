{-# LANGUAGE TemplateHaskell #-}

module Account.Savings where

import Account.Classes
import Aliases
import Control.Lens
import Data.DateTime

data SavingsAccount = SavingsAccount
  { _sId :: String
  , _sName :: String
  , _sBalance :: Amount
  , _sInterestRate :: InterestRate
  , _sDateOpen :: DateTime
  , _sDateClosed :: Maybe DateTime
  }

makeLenses ''SavingsAccount

instance Show SavingsAccount where
  show (SavingsAccount i n b _ _ _) = i ++ " - " ++ n ++ ": $" ++ (show b)

instance Account SavingsAccount where
  accountId = sId
  name = sName
  balance = sBalance
  dateClosed = sDateClosed

instance InterestBearingAccount SavingsAccount where
  interestRate = sInterestRate
