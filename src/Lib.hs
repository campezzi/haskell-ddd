module Lib where

import Account.Checking
import Account.Savings
import Data.DateTime
import Data.Validation (AccValidation(..))

testDate :: DateTime
testDate = fromGregorian 2017 6 19 22 30 00

testCheckingAccount :: CheckingAccount
testCheckingAccount =
  CheckingAccount "CHK-124" "Checking Account" 500 testDate Nothing

testSavingsAccount :: AccValidation [ValidationError] SavingsAccount
testSavingsAccount = savingsAccount "SAVINGS-001" 0.02 testDate
