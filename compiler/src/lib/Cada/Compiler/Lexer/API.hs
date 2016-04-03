--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Lexer.API (
    LexerAPI
) where

--------------------------------------------------------------------------------

import Servant.API

import Cada.Compiler.Utility.UUID
import Cada.Compiler.Lexer.Token
import Cada.Compiler.Lexer.Types
import Cada.Compiler.Lexer.Range

--------------------------------------------------------------------------------

-- | The lexer API.
type LexerAPI =
      "lexer" :>
      Capture "project" UUID :>
      "tokens" :>
      Capture "file" UUID :>
      Post '[JSON] (TokensRes Token)
 :<|> "lexer" :>
      Capture "project" UUID :>
      "file" :>
      "update" :>
      Capture "file" UUID :>
      ReqBody '[JSON] TextUpdate :>
      Post '[JSON] String

--------------------------------------------------------------------------------
