{-# LANGUAGE DeriveFunctor #-}

module AccountRepositoryF where

import Control.Monad.Free
import Data.Map (Map, delete, fromList, insert, lookup)
import Prelude hiding (lookup)

data AnAccount = AnAccount
  { accNumber :: String
  , accName :: String
  } deriving (Show)

data AccountRepoF next
  = Query String
          (AnAccount -> next)
  | Store AnAccount
          next
  | Remove String
           next
  deriving (Functor)

type AccountRepo = Free AccountRepoF

query :: String -> AccountRepo AnAccount
query accountNumber = liftF $ Query accountNumber id

store :: AnAccount -> AccountRepo ()
store account = liftF $ Store account ()

remove :: String -> AccountRepo ()
remove accountNumber = liftF $ Remove accountNumber ()

update :: String -> (AnAccount -> AnAccount) -> AccountRepo ()
update accountNumber f = do
  account <- query accountNumber
  store (f account)

open :: String -> String -> AccountRepo AnAccount
open number name = do
  store $ AnAccount number name
  account <- query number
  return account

--
anAccount :: AnAccount
anAccount = AnAccount "123" "Thiago"

--
render :: (Show r) => AccountRepo r -> String
render (Free (Query _ fr)) = "query\n" ++ render (fr (AnAccount "" ""))
render (Free (Store _ r)) = "store\n" ++ render r
render (Free (Remove _ r)) = "delete\n" ++ render r
render (Pure _) = "return\n"

pretty :: (Show r) => AccountRepo r -> IO ()
pretty = putStr . render

--
class AccountRepository r where
  apply :: AccountRepo a -> r -> r

data MapRepo =
  MapRepo (Map String AnAccount)
  deriving (Show)

instance AccountRepository MapRepo where
  apply action r@(MapRepo repo) =
    case action of
      Free (Query accountNumber next) ->
        case lookup accountNumber repo of
          Nothing -> r
          Just account -> apply (next account) r
      Free (Store account next) -> apply next updatedRepo
        where updatedRepo = MapRepo $ insert (accNumber account) account repo
      Free (Remove accountNumber next) -> apply next updatedRepo
        where updatedRepo = MapRepo $ delete accountNumber repo
      Pure _ -> r

--
emptyRepo :: MapRepo
emptyRepo = MapRepo (fromList [])

testComp :: AccountRepo AnAccount
testComp = do
  open "123" "Thiago"
  open "456" "Campezzi"
