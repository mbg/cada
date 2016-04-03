--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Lexer.Pos where

--------------------------------------------------------------------------------

import Control.Monad
import Data.Aeson

--------------------------------------------------------------------------------

-- | Represents a position, consisting of a line and a column (both start at 1).
data Pos = Pos {
    posLine   :: Int,
    posColumn :: Int
} deriving Show

instance ToJSON Pos where
    toJSON Pos{..} =
        object [ "line"   .= posLine
               , "column" .= posColumn
               ]

instance FromJSON Pos where
    parseJSON (Object v) =
        Pos <$> v .: "line"
            <*> v .: "column"
    parseJSON _ = mzero

--------------------------------------------------------------------------------

data FilePos = FilePos {
    filePosSource :: FilePath,
    filePosValue  :: Pos
} deriving Show

--------------------------------------------------------------------------------
