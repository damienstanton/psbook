module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)

-- type aliases for records
type Entry =
    { firstName :: String
    , lastName :: String
    , address :: Address
    }

type Address = 
    { street :: String
    , city :: String
    , state :: String
    }

-- composite type
type AddressBook = List Entry

-- some functions
showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
    entry.firstName <> ": " <>
    showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
    addr.city <> ", " <>
    addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
-- eta conversion:
-- insertEntry entry book = Cons entry book
-- insertEntry entry = Cons entry
-- finally becomes:
insertEntry = Cons 

-- querying the address book
findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
    where
        filterEntry :: Entry -> Boolean
        filterEntry entry = entry.firstName == firstName && entry.lastName == lastName