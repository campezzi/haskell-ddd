module Main where

import Account.Classes
import Data.Validation (AccValidation(..))
import Lib

main :: IO ()
main = do
  putStrLn "Opening savings account..."
  let result = testSavingsAccount
  case result of
    AccFailure errors -> do
      putStrLn "There were errors opening the account:"
      putStrLn $ show errors
    AccSuccess account -> do
      putStrLn "Account opened successfully!"
      putStrLn $ show account
