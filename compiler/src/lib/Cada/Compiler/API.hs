--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.API (
    module Cada.Compiler.Project.API,
    module Cada.Compiler.Lexer.API,

    API,
    compilerApi
) where

--------------------------------------------------------------------------------

import Data.Proxy

import Servant.API

import Cada.Compiler.Project.API
import Cada.Compiler.Lexer.API

--------------------------------------------------------------------------------

type API = ProjectAPI :<|> LexerAPI

compilerApi :: Proxy API
compilerApi = Proxy

--------------------------------------------------------------------------------
