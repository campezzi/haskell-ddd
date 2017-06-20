{-# LANGUAGE RankNTypes #-}

module Account.Classes where

import Aliases
import Control.Lens
import Data.DateTime

data AccountError
  = InsufficientFunds
  | AccountClosed
  deriving (Eq, Show)

class Account a where
  {-# MINIMAL (accountId, name, balance, dateClosed) #-}
  accountId :: Lens' a String
  name :: Lens' a String
  balance :: Lens' a Amount
  dateClosed :: Lens' a (Maybe DateTime)

isClosed :: Account a => a -> Bool
isClosed account
  | view dateClosed account == Nothing = False
  | otherwise = True

credit :: Account a => a -> Amount -> Either AccountError a
credit account amount
  | isClosed account = Left AccountClosed
  | otherwise = Right $ over balance (+ amount) account

debit :: Account a => a -> Amount -> Either AccountError a
debit account amount
  | isClosed account = Left AccountClosed
  | view balance account < amount = Left InsufficientFunds
  | otherwise = Right $ over balance (subtract amount) account

transfer ::
     (Account a, Account b) => a -> b -> Amount -> Either AccountError (a, b)
transfer source destination amount = do
  source' <- debit source amount
  destination' <- credit destination amount
  return (source', destination')

class InterestBearingAccount a where
  {-# MINIMAL (interestRate) #-}
  interestRate :: Lens' a InterestRate
