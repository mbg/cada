--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Lexer.Types where

--------------------------------------------------------------------------------

import Data.Aeson

import Cada.Compiler.Utility.UUID

--------------------------------------------------------------------------------

data TokensRes t
    = TokensRes {
        tokens :: [t],
        newVersion :: UUID
    }
    | LexerErrRes {
        lexerError :: String
    }

instance ToJSON t => ToJSON (TokensRes t) where
    toJSON TokensRes{..} =
        object [ "tokens"  .= toJSON tokens
               , "version" .= toJSON newVersion
               ]
    toJSON (LexerErrRes err) = object [ "error"  .= toJSON err ]

instance FromJSON t => FromJSON (TokensRes t) where
    parseJSON (Object v) = undefined

data UpdateFileReq = UpdateFileReq {

}

--------------------------------------------------------------------------------
