module ErrorList where

import Data.Semigroup

newtype ErrorList a =
  ErrorList [a]

instance Semigroup (ErrorList a) where
  ErrorList x <> ErrorList y = ErrorList $ x <> y

instance (Show a) => Show (ErrorList a) where
  show (ErrorList []) = ""
  show (ErrorList (x:xs)) = "- " ++ (show x) ++ "\n" ++ (show (ErrorList xs))
