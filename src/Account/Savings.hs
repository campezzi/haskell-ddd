{-# LANGUAGE TemplateHaskell #-}

module Account.Savings
  ( SavingsAccount
  , ValidationError
  , savingsAccount
  ) where

import Account.Classes
import Aliases
import Control.Lens
import Data.DateTime
import Data.Validation (AccValidation(..))

data ValidationError
  = AccountIdTooShort
  | NegativeInterest

instance Show ValidationError where
  show AccountIdTooShort = "Account ID must be at least 10 characters long"
  show NegativeInterest = "Interest Rate must be positive"

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

validateId :: String -> AccValidation [ValidationError] String
validateId aId
  | length aId < 10 = AccFailure [AccountIdTooShort]
  | otherwise = AccSuccess aId

validateInterestRate ::
     InterestRate -> AccValidation [ValidationError] InterestRate
validateInterestRate interest
  | interest <= 0 = AccFailure [NegativeInterest]
  | otherwise = AccSuccess interest

savingsAccount ::
     String
  -> InterestRate
  -> DateTime
  -> AccValidation [ValidationError] SavingsAccount
savingsAccount aId interest date =
  SavingsAccount <$> validateId aId <*> AccSuccess "Savings Account" <*>
  AccSuccess 0 <*>
  validateInterestRate interest <*>
  AccSuccess date <*>
  AccSuccess Nothing
