module Account where

import Address
import Bank
import Data.DateTime

type Amount = Double

data Account = Account
  { accountName :: String
  , bank :: Bank
  , address :: Address
  , dateOpen :: DateTime
  , dateClosed :: Maybe DateTime
  , balance :: Amount
  }

instance Show Account where
  show = do
    n <- accountName
    b <- show . bank
    t <- show . balance
    return $ n ++ " (" ++ b ++ "): $" ++ t

data AccountServiceError
  = NotEnoughFunds Account
  | AccountClosed Account
  deriving (Show)

--
isClosed :: Account -> Bool
isClosed account
  | dateClosed account == Nothing = False
  | otherwise = True

debit :: Account -> Amount -> Either AccountServiceError Account
debit account amount
  | isClosed account = Left (AccountClosed account)
  | currentBalance < amount = Left (NotEnoughFunds account)
  | otherwise = Right $ account {balance = (currentBalance - amount)}
  where
    currentBalance = balance account

credit :: Account -> Amount -> Either AccountServiceError Account
credit account amount
  | isClosed account = Left (AccountClosed account)
  | otherwise = Right $ account {balance = currentBalance + amount}
  where
    currentBalance = balance account

transfer ::
     Account
  -> Account
  -> Amount
  -> Either AccountServiceError (Account, Account)
transfer source destination amount = do
  updatedSource <- debit source amount
  updatedDestination <- credit destination amount
  return (updatedSource, updatedDestination)

close :: Account -> DateTime -> Account
close account when = account {dateClosed = (Just when)}
