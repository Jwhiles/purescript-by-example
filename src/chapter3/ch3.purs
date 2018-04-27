module Ch3 where

import Control.Monad.Eff.Console
import Prelude
import Data.AddressBook
import Data.Maybe (fromMaybe)
import Data.Newtype

address :: Address
address = Address { street: "123 Fake St.", city: "Faketown", state: "CA" }

entry :: Entry
entry = Entry { firstName: "John", lastName: "Smiles", address: address }


ch3 = do
  log $ tt testBoi
  log "Chapter 3 exercises"
  log $ fromMaybe "Not found" $ printEntry "John" "Smiles" book1
  log $ fromMaybe "Not found" $ printEntry "no one" "Smiles" book1
  logShow $ isNamePresent "John" book1
  logShow $ isNamePresent "francine" book1
  log $ fromMaybe "Not found" $ printEntry "John" "Smiles" book2
  log $ fromMaybe "Not found" $ printEntry "John" "Smiles" $ removeDuplicates book2
  {-- logShow $ book2 --}
    where 
      book1 = insertEntry entry emptyBook
      book2 = insertEntry entry book1


newtype Test = Test { firstName :: String }

derive instance newtypeTest :: Newtype Test _
{-- derive instance newtypeShow :: Show Test --}

testBoi = Test { firstName: "john" }

tt = (\x -> x.firstName) <<< unwrap 
