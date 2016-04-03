--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Utility.UUID (
    module Data.UUID
) where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Aeson
import Data.SafeCopy
import Data.UUID

import Servant.API

--------------------------------------------------------------------------------

instance ToJSON UUID where
    toJSON = String . toText

instance FromJSON UUID where
    parseJSON (String v) = case fromText v of
        Nothing -> fail "Can't parse UUID."
        Just t  -> pure t
    parseJSON _          = mzero

instance ToHttpApiData UUID where
    toQueryParam = toText

instance FromHttpApiData UUID where
    parseQueryParam v = case fromText v of
        Nothing -> Left "Can't parse UUID."
        Just t  -> return t

--------------------------------------------------------------------------------
