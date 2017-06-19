module Lib where

import Account.Checking
import Data.DateTime

testDate :: DateTime
testDate = fromGregorian 2017 6 19 22 30 00

testCheckingAccount :: CheckingAccount
testCheckingAccount =
  CheckingAccount "XHQ-124" "Test Account" 500 testDate Nothing
