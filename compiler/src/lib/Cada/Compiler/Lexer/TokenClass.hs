--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Lexer.TokenClass where

--------------------------------------------------------------------------------

import Control.Monad (mzero)
import Data.Aeson

--------------------------------------------------------------------------------

-- | Token class for whitespace-like tokens.
data Whitespace
    -- | Some number of spaces.
    = Space Int
    -- | A tab character.
    | Tab
    -- | A new-line character.
    | NewLine { isWindowsNL :: Bool }
    -- | The end-of-file marker.
    | EoF
    deriving Show

instance ToJSON Whitespace where
    toJSON (Space n)     = object [ "space" .= toJSON n ]
    toJSON (NewLine win) =
        object [ "linebreak" .= object [ "windows" .= toJSON win ] ]
    toJSON Tab           = String "tab"
    toJSON EoF           = String "eof"

instance FromJSON Whitespace where
    parseJSON (String "tab") = pure Tab
    parseJSON (String "eof") = pure EoF
    parseJSON (Object v) = undefined
    parseJSON _ = mzero

--------------------------------------------------------------------------------

-- | Represents classes of tokens.
data TokenClass
    = Whitespace Whitespace
    | Fragment Char
    deriving Show

instance ToJSON TokenClass where
    toJSON (Whitespace w) = toJSON w
    toJSON (Fragment xs) = object [ "fragment" .= toJSON xs ]

instance FromJSON TokenClass where
    parseJSON (Object v) = undefined

--------------------------------------------------------------------------------
