--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Project.File where

--------------------------------------------------------------------------------

import Data.Aeson
import qualified Data.Text as T

import Cada.Compiler.Utility.UUID (UUID)
import Cada.Compiler.Lexer.TextBuffer
import Cada.Compiler.Lexer.Token
import Cada.Compiler.Lexer

--------------------------------------------------------------------------------

data ProjFile = ProjFile {
    projFileVersion    :: UUID,
    projFileTextBuffer :: TextBuffer,
    projFileTokens     :: [Token]
}

emptyProjFile :: UUID -> ProjFile
emptyProjFile uuid = ProjFile {
    projFileVersion = uuid,
    projFileTextBuffer = def,
    projFileTokens = []
}

newFileFromText :: UUID -> T.Text -> ProjFile
newFileFromText uuid txt = (emptyProjFile uuid) {
    projFileTextBuffer = fromText txt,
    projFileTokens = case runLexer "" txt of
        Left err -> []
        Right ts -> ts
}

--------------------------------------------------------------------------------
