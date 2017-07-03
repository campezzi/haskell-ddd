module LoanApplication where

import Data.DateTime (DateTime, fromGregorian)

data Applied

data Approved

data Enriched

data LoanApplication a = LoanApplication
  { date :: DateTime
  , name :: String
  , purpose :: String
  , repayIn :: Int
  , actualRepaymentYears :: Maybe Int
  , startDate :: Maybe DateTime
  , loanNo :: Maybe String
  , emi :: Maybe Double
  } deriving (Show)

applyLoan :: String -> String -> Int -> DateTime -> LoanApplication Applied
applyLoan name' purpose' repayIn' date' =
  LoanApplication date' name' purpose' repayIn' Nothing Nothing Nothing Nothing

approve :: LoanApplication Applied -> Maybe (LoanApplication Approved)
approve loan =
  Just
    loan
    { loanNo = Just "SomeLoanNumber"
    , actualRepaymentYears = Just 15
    , startDate = Just $ fromGregorian 2017 07 03 23 10 00
    }

enrich :: LoanApplication Approved -> Maybe (LoanApplication Enriched)
enrich loan = do
  actualRepaymentYears' <- actualRepaymentYears loan
  return loan {emi = Just $ fromIntegral actualRepaymentYears'}
