--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Lexer.Server where

--------------------------------------------------------------------------------

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import qualified Data.Map as M

import Servant.API
import Servant.Server

import Cada.Compiler.Utility.UUID
import Cada.Compiler.Project.File
import Cada.Compiler.Project.Server
import Cada.Compiler.Project.Types
import Cada.Compiler.Lexer.API
import Cada.Compiler.Lexer.Range
import Cada.Compiler.Lexer.TextBuffer
import Cada.Compiler.Lexer.Token
import Cada.Compiler.Lexer.Types
import Cada.Compiler.State

--------------------------------------------------------------------------------

fileExistsCheck :: TVar Project -> UUID -> CompSrvM (TVar ProjFile)
fileExistsCheck proj uuid = do
    Proj{..} <- liftIO $ readTVarIO proj

    case M.lookup uuid projFiles of
        Nothing   -> throwE err404
        Just file -> return file

--------------------------------------------------------------------------------

-- | `retrieveTokens proj uuid` retrives the tokens for the file whose globally
--   unique id is `uuid`.
retrieveTokens :: UUID -> UUID -> CompSrvM (TokensRes Token)
retrieveTokens proj uuid = do
    p <- projectExistsCheck proj
    f <- fileExistsCheck p uuid

    ProjFile{..} <- liftIO $ readTVarIO f

    return $ TokensRes projFileTokens projFileVersion

updateFile :: UUID -> UUID -> TextUpdate -> CompSrvM String
updateFile proj uuid updt = do
    p <- projectExistsCheck proj
    f <- fileExistsCheck p uuid

    liftIO $ atomically $ do
        file <- readTVar f

        let
            result = update updt (projFileTextBuffer file)

        writeTVar f $ file { projFileTextBuffer = result }

        return (show result)

--------------------------------------------------------------------------------

-- | `lexerServer` handles requests related to the lexer API.
lexerServer :: CompSrv LexerAPI
lexerServer = retrieveTokens
         :<|> updateFile

--------------------------------------------------------------------------------
