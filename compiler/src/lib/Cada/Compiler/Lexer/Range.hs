--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Lexer.Range (
    module Cada.Compiler.Lexer.Pos,

    Span(..),
    Range(..),
    TextUpdate(..)
) where

--------------------------------------------------------------------------------

import Data.Aeson
import qualified Data.Text as T

import Cada.Compiler.Lexer.Pos

--------------------------------------------------------------------------------

data Span = Span {
    spanStart  :: Int,          -- ^ The offset into the document at
                                --   which the span starts.
    spanLength :: Int           -- ^ The length of the span.
}

instance ToJSON Span where
    toJSON Span{..} =
        object [ "start"  .= spanStart
               , "length" .= spanLength
               ]

instance FromJSON Span where
    parseJSON (Object v) =
        Span <$> v .: "start"
             <*> v .: "length"

-- | Represents a text range in a document.
data Range = Range {
    rangeStart  :: Pos,
    rangeEnd    :: Pos
} deriving Show

instance ToJSON Range where
    toJSON Range{..} =
        object [ "start"  .= rangeStart
               , "end"    .= rangeEnd
               ]

instance FromJSON Range where
    parseJSON (Object v) =
        Range <$> v .: "start"
              <*> v .: "end"

--------------------------------------------------------------------------------

data TextUpdate = TextUpdate {
    updatedRange :: Range,
    updatedValue :: T.Text
} deriving Show

instance ToJSON TextUpdate where
    toJSON TextUpdate{..} =
        object [ "range" .= toJSON updatedRange
               , "text"  .= toJSON updatedValue
               ]

instance FromJSON TextUpdate where
    parseJSON (Object v) =
        TextUpdate <$> v .: "range"
                   <*> v .: "text"

--------------------------------------------------------------------------------
