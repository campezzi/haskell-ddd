module Bank where

data Bank = Bank
  { name :: String
  }

instance Show Bank where
  show = name
