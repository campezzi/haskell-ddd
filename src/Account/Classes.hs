module Account.Classes where

import Aliases
import Data.DateTime

data AccountError
  = InsufficientFunds
  | AccountClosed
  deriving (Eq, Show)

class Account a where
  {-# MINIMAL (accountId, name, balance, updateBalance, close)
            , (isOpen | isClosed) #-}
  accountId :: a -> String
  name :: a -> String
  balance :: a -> Amount
  updateBalance :: a -> (Amount -> Amount -> Amount) -> Amount -> a
  isOpen :: a -> Bool
  isClosed :: a -> Bool
  credit :: a -> Amount -> Either AccountError a
  debit :: a -> Amount -> Either AccountError a
  transfer :: Account b => a -> b -> Amount -> Either AccountError (a, b)
  close :: a -> DateTime -> Either AccountError a
  isOpen = not . isClosed
  isClosed = not . isOpen
  credit account amount
    | isClosed account = Left AccountClosed
    | otherwise = Right $ updateBalance account (+) amount
  debit account amount
    | isClosed account = Left AccountClosed
    | balance account < amount = Left InsufficientFunds
    | otherwise = Right $ updateBalance account subtract amount
  transfer source destination amount = do
    source' <- debit source amount
    destination' <- credit destination amount
    return (source', destination')

class InterestBearingAccount a where
  {-# MINIMAL (interestRate) #-}
  interestRate :: a -> InterestRate
