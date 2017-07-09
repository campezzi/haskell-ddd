{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module AccountRepositoryF where

import Control.Monad.Free
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Map (Map, delete, empty, insert, lookup)
import Prelude hiding (lookup)

data AnAccount = AnAccount
  { accNumber :: String
  , accName :: String
  } deriving (Show)

data AccountRepoActionF next
  = Query String
          (AnAccount -> next)
  | Store AnAccount
          next
  | Remove String
           next
  deriving (Functor)

type AccountRepoAction = Free AccountRepoActionF

query :: String -> AccountRepoAction AnAccount
query accountNumber = liftF $ Query accountNumber id

store :: AnAccount -> AccountRepoAction ()
store account = liftF $ Store account ()

remove :: String -> AccountRepoAction ()
remove accountNumber = liftF $ Remove accountNumber ()

update :: String -> (AnAccount -> AnAccount) -> AccountRepoAction ()
update accountNumber f = do
  account <- query accountNumber
  store (f account)

open :: String -> String -> AccountRepoAction AnAccount
open number name = do
  store $ AnAccount number name
  account <- query number
  return account

--
class AccountRepository r where
  apply :: AccountRepoAction a -> r a

--
type InMemoryRepo = Map String AnAccount

type InMemoryRepoState = StateT InMemoryRepo (Either [String])

instance AccountRepository InMemoryRepoState where
  apply action =
    case action of
      Free (Query accountNumber next) -> do
        maybeAccount <- gets (lookup accountNumber)
        case maybeAccount of
          Nothing -> lift $ Left ["Account does not exist"]
          Just account -> apply (next account)
      Free (Store account next) ->
        (modify $ insert (accNumber account) account) >> apply next
      Free (Remove accountNumber next) ->
        (modify $ delete accountNumber) >> apply next
      Pure a -> return a

--
emptyRepo :: InMemoryRepo
emptyRepo = empty

anAccount :: AnAccount
anAccount = AnAccount "123" "Thiago"

changeName :: String -> AnAccount -> AnAccount
changeName name account = account {accName = name}

testCompS :: AccountRepoAction ()
testCompS = do
  open "123" "Thiago"
  open "456" "Campezzi"
  remove "456"
  open "789" "Mr. Bean"
  update "123" (changeName "Mr. Happy")

testCompF :: AccountRepoAction ()
testCompF = do
  query "abc"
  testCompS

testApplyS :: Either [String] ((), InMemoryRepo)
testApplyS = runStateT (apply testCompS) emptyRepo

testApplyF :: Either [String] ((), InMemoryRepo)
testApplyF = runStateT (apply testCompF) emptyRepo
