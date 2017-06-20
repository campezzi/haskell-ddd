module Lib where

import Account.Checking
import Account.Savings
import Data.DateTime

testDate :: DateTime
testDate = fromGregorian 2017 6 19 22 30 00

testCheckingAccount :: CheckingAccount
testCheckingAccount =
  CheckingAccount "CHK-124" "Checking Account" 500 testDate Nothing

testSavingsAccount :: SavingsAccount
testSavingsAccount =
  SavingsAccount "SAV-001" "Savings Account" 100 0.05 testDate Nothing
