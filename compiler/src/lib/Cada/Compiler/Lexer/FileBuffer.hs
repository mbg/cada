--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Lexer.FileBuffer (
    module Data.Default.Class,

    FileBuffer(..),

    index
) where

--------------------------------------------------------------------------------

import           Data.Default.Class
import           Data.Foldable                      (toList)
import qualified Data.Sequence                      as Seq

--------------------------------------------------------------------------------

-- | A `FileBuffer` represents the contents of a file, grouped by line.
newtype FileBuffer a = FileBuffer { fileBuffer :: Seq.Seq a }

instance Default (FileBuffer a) where
    def = FileBuffer Seq.empty

instance Show a => Show (FileBuffer a) where
    show = show . zip [1..] . toList . fileBuffer

instance Monoid (FileBuffer a) where
    mempty = def

    mappend (FileBuffer l) (FileBuffer r) = FileBuffer (l Seq.>< r)

-- | \O(log(min(i,n-1)))\. `index buffer line` retrieves the row at index
-- `line` from `buffer`.
index :: FileBuffer a -> Int -> Maybe a
index FileBuffer{..} line
    | line > Seq.length fileBuffer = Nothing
    | otherwise                    = Just $ Seq.index fileBuffer (line - 1)

--------------------------------------------------------------------------------
