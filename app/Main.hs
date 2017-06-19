module Main where

import Account.Checking
import Account.Classes
import Data.DateTime
import Lib

main :: IO CheckingAccount
main = do
  t <- getCurrentTime
  a <- pure $ close testCheckingAccount t
  case a of
    Left _ -> return testCheckingAccount
    Right account -> return account
