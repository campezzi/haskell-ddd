module Main where

import Account.Savings (savingsAccount)
import Aliases
import Data.DateTime (getCurrentTime)
import Data.Validation (AccValidation(..))
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "Opening savings account..."
  putStr "Account ID: "
  now <- getCurrentTime
  aId <- getLine
  interest <- getInterest
  case savingsAccount aId interest now of
    AccFailure errors -> do
      putStrLn "There were errors opening the account:"
      putStr $ show errors
    AccSuccess account -> do
      putStrLn "Account opened successfully!"
      putStrLn $ show account

getInterest :: IO InterestRate
getInterest = do
  putStr "Interest Rate: "
  interest <- getLine
  case readMaybe interest of
    Just interest' -> return interest'
    Nothing -> do
      putStrLn "Could not parse interest rate. Please try a decimal number."
      getInterest
