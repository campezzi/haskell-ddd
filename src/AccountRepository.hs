module AccountRepository where

import Data.Map (Map, (!), fromList, insert, lookup)
import Prelude hiding (lookup)

data MyAccount = MyAccount
  { accNumber :: String
  , accBalance :: Double
  } deriving (Show)

class AccountRepository a where
  query :: a -> String -> Either [String] (Maybe MyAccount)
  store :: a -> MyAccount -> Either [String] (MyAccount, a)
  balance :: a -> String -> Either [String] Double
  balance repo accountNumber =
    case query repo accountNumber of
      Left errors -> Left errors
      Right Nothing -> Left ["No account with this number"]
      Right (Just account) -> Right $ accBalance account

data InMemoryAccountRepository =
  Repo (Map String MyAccount)
  deriving (Show)

instance AccountRepository InMemoryAccountRepository where
  query (Repo repo) accountNumber = Right $ lookup accountNumber repo
  store (Repo repo) account =
    Right $ (updatedRepo ! accountNumber, Repo updatedRepo)
    where
      accountNumber = accNumber account
      updatedRepo = insert accountNumber account repo

go :: Either [String] (Maybe MyAccount, Maybe MyAccount, Double)
go = do
  let repo = Repo (fromList [])
  acc <- query repo "thiago"
  (_, repo') <- store repo (MyAccount "thiago" 124.5)
  acc' <- query repo' "thiago"
  bal <- balance repo' "thiago"
  return (acc, acc', bal)
