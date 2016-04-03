--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Server where

--------------------------------------------------------------------------------

import Network.Wai

import Servant.API
import Servant.Server

import Cada.Compiler.API
import Cada.Compiler.State
import Cada.Compiler.Project.Server
import Cada.Compiler.Lexer.Server

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

compilerServer :: CompStVar -> Server API
compilerServer st = enter (stateToHandler st) (projectServer :<|> lexerServer)

compilerApp :: CompStVar -> Application
compilerApp st = serve compilerApi (compilerServer st)

--------------------------------------------------------------------------------
