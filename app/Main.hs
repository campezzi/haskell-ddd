module Main where

import Account.Classes
import Lib

main :: IO ()
main = do
  let result = transfer testCheckingAccount testSavingsAccount 300
  putStrLn $ either (("Error: " ++) . show) show result
