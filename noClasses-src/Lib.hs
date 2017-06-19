module Lib where

import Account
import Address
import Bank
import Data.DateTime

nab :: Bank
nab = Bank "NAB"

home :: Address
home = Address "St. Kilda Road" 213

bday :: DateTime
bday = fromGregorian 2017 6 18 20 00 00

elon :: Account
elon = Account "Elon Musk" nab home bday Nothing 10000

thiago :: Account
thiago = elon {accountName = "Thiago Campezzi", balance = 0}
