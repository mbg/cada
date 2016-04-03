--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Lexer.Token (
    module Cada.Compiler.Lexer.TokenClass,

    Token(..)
) where

--------------------------------------------------------------------------------

import Data.Aeson

import Cada.Compiler.Lexer.Pos
import Cada.Compiler.Lexer.TokenClass

--------------------------------------------------------------------------------

data Token = Tkn {
    tknClass :: TokenClass,
    tknPos   :: Pos
} deriving Show

instance ToJSON Token where
    toJSON Tkn{..} =
        object [ "position" .= toJSON tknPos
               , "class"    .= toJSON tknClass
               ]

instance FromJSON Token where
    parseJSON (Object v) =
        Tkn <$> v .: "class"
            <*> v .: "position"

--------------------------------------------------------------------------------
