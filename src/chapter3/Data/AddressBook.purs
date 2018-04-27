module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, some, nub)
import Data.Maybe (Maybe)
import Data.Foldable (foldr)
import Data.Newtype



newtype Address = Address
  { street :: String
  , city   :: String
  , state  :: String
  }


newtype Entry = Entry
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }


type AddressBook = List Entry

showAddress :: Address -> String
showAddress (Address { street, city, state }) = 
  street <> ", " <> city <> ", " <> state

showEntry :: Entry -> String
showEntry (Entry { lastName, firstName, address }) = 
  lastName <> ", " <> firstName <> ": " <> showAddress address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry f l = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry (Entry { firstName, lastName }) = 
      firstName == f && lastName == l


printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = 
  map showEntry (findEntry firstName lastName book)

-- exercises
findAddress :: String -> String -> String -> AddressBook -> Maybe Entry
findAddress street city state = head <<< filter filterAddresses
  where 
        filterAddresses :: Entry -> Boolean
        filterAddresses (Entry { address: (Address a) }) = 
                              a.street == street &&
                              a.city == city &&
                              a.state == state


-- this function could be more efficient - how do we early return if we find
isNamePresent :: String -> AddressBook -> Boolean
isNamePresent name addr = foldr test false addr
  where 
        test (Entry { firstName: n }) acc = n == name || acc



derive instance newtypeAddress :: Newtype Address _
derive instance eqAddress :: Eq Address 

derive instance newtypeEntry :: Newtype Entry _
derive instance eqEntry :: Eq Entry 

-- deriving these instances allows me to simply use nub because I have EQ.
-- but adds extra complexity to all the other code where I have to unwrap the type

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates addrb = nub addrb

