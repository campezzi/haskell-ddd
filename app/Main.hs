module Main where

import Account
import Data.DateTime
import Lib

main :: IO Account
main = do
  t <- getCurrentTime
  return $ close thiago t
