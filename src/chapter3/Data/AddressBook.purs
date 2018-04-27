module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, some, nub)
import Data.Maybe (Maybe)
import Data.Foldable (foldr)
import Data.Newtype



tt = (\x -> x.firstName) <<< unwrap 

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
showAddress =
  (\addr -> addr.street <> ", " <> addr.city <> ", " <> addr.state) <<< unwrap

showEntry :: Entry -> String
showEntry =
  (\entry -> entry.lastName <> ", " <> entry.firstName <> ": " <>
  showAddress entry.address) <<< unwrap

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry = (\entry -> entry.firstName == firstName && entry.lastName ==
      lastName) <<< unwrap


printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = 
  map showEntry (findEntry firstName lastName book)

-- exercises
{-- findAddress :: String -> String -> String -> AddressBook -> Maybe Entry --}
{-- findAddress street city state = head <<< filter filterAddresses --}
{--   where --} 
{--         filterAddresses :: Entry -> Boolean --}
{--         filterAddresses { address: a } = --} 
{--           a.street == street && --}
{--           a.city == city && --}
{--           a.state == state --}

findAddress :: String -> String -> String -> AddressBook -> Maybe Entry
findAddress street city state = head <<< filter filterAddresses
  where 
        filterAddresses :: Entry -> Boolean
        filterAddresses = unwrap >>> (\{ address: wa } -> 
                          (unwrap >>> (\a -> 
                              a.street == street &&
                              a.city == city &&
                              a.state == state)) wa)


-- this function could be more efficient - how do we early return if we find
isNamePresent :: String -> AddressBook -> Boolean
isNamePresent name addr = foldr test false addr
  where 
        test e acc = (unwrap >>> \{ firstName : n } -> n == name || acc) e
    {-- test { firstName: n } a = n == name || a --}



derive instance newtypeAddress :: Newtype Address _
derive instance eqAddress :: Eq Address 

derive instance newtypeEntry :: Newtype Entry _
derive instance eqEntry :: Eq Entry 

-- deriving these instances allows me to simply use nub because I have EQ.
-- but adds extra complexity to all the other code. Can this be simplified?

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates addrb = nub addrb

