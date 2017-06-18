module Address where

data Address = Address
  { street :: String
  , number :: Int
  }

instance Show Address where
  show (Address s n) = s ++ ", " ++ (show n)
