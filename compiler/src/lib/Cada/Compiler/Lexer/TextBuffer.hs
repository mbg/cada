--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Lexer.TextBuffer (
    module Data.Default.Class,
    module Cada.Compiler.Lexer.Range,

    TextBuffer(..),
    FileBuffer(..),
    LineSize(..),
    LineBuffer(..),
    TextStream(..),

    fromText,
    splitAfter,
    update
) where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Default.Class
import Data.FingerTree as FT
import Data.Foldable
import qualified Data.IntMap as IM
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import Text.Megaparsec.Prim

import Cada.Compiler.Lexer.Range
import Cada.Compiler.Lexer.FileBuffer

--------------------------------------------------------------------------------

type TextBuffer = FileBuffer LineBuffer

--------------------------------------------------------------------------------

-- | `LineSize` represents the number of characters in a line.
newtype LineSize = LineSize { lineSize :: Int }

instance Monoid LineSize where
    -- the length of an empty line
    mempty = LineSize 0

    -- the sum of two line lengths
    x `mappend` y = LineSize (lineSize x + lineSize y)

instance Measured LineSize T.Text where
    measure = LineSize . T.length

-- | `LineBuffer` represents the characters in a line.
data LineBuffer = LineBuffer { lineBuffer :: FingerTree LineSize T.Text }

instance Default LineBuffer where
    def = LineBuffer empty

instance Show LineBuffer where
    show = show . toList . lineBuffer

instance Monoid LineBuffer where
    mempty = def

    mappend (LineBuffer l) (LineBuffer r) = LineBuffer (l >< r)

--------------------------------------------------------------------------------

isNull :: LineBuffer -> Bool
isNull = FT.null . lineBuffer

-- | `toLineBuffer v` converts `v` to a line buffer.
toLineBuffer :: T.Text -> LineBuffer
toLineBuffer v
    | T.null v = def
    | otherwise = LineBuffer (singleton v)

-- | `toLineBuffers v` converts some text `v` to a list of line buffers.
toLineBuffers :: T.Text -> [LineBuffer]
toLineBuffers v
    | T.null v  = []
    | otherwise = case T.uncons xs of
        Nothing     -> [toLineBuffer x]
        Just (y,ys) -> toLineBuffer (T.snoc x y) : toLineBuffers ys -- TODO: avoid `snoc`
    where
        (x,xs) = T.span (/= '\n') v

-- | `fromText v` converts some text `v` to a file buffer.
fromText :: T.Text -> FileBuffer LineBuffer
fromText v
    | T.null v  = def
    | otherwise = FileBuffer (Seq.fromList ls)
        where
            ls = toLineBuffers v

lineToText :: LineBuffer -> T.Text
lineToText (LineBuffer buffer) = mconcat (toList buffer)

append :: LineBuffer -> LineBuffer -> LineBuffer
append (LineBuffer b1) (LineBuffer b2) = LineBuffer (b1 >< b2)

-- | `splitAfter n b` splits a text buffer after `n` characters.
splitAfter :: Int -> LineBuffer -> (LineBuffer, LineBuffer)
splitAfter n (LineBuffer buffer) =
    -- split the finger tree into two -- O(log(min(n1,n2)))
    let (left, right) = split ((> n) . lineSize) buffer
    -- inspect the left end of `right` -- O(1)
    in case viewl right of
        -- if the sequence is empty, we are trying to index into a location
        -- which may not exist...
        EmptyL   -> (LineBuffer left, LineBuffer right)
        --
        txt :< r -> let m        = n - lineSize (measure left)
                        (xs, ys) = T.splitAt m txt
                        left'    = LineBuffer left `append` toLineBuffer xs
                        right'   = toLineBuffer ys `append` LineBuffer r
                    in (left',right')

-- `update data buffer` updates `buffer` with `data`.
update :: TextUpdate -> TextBuffer -> TextBuffer
update TextUpdate{..} FileBuffer{..}
    | posLine (rangeStart updatedRange) > Seq.length fileBuffer =
        mconcat [FileBuffer fileBuffer, fromText updatedValue]
    | otherwise = mconcat [ls, fromText m, rs]
    where
        (left, _)  = Seq.splitAt (posLine $ rangeStart updatedRange) fileBuffer
        (_, right) = Seq.splitAt (posLine (rangeEnd updatedRange) - 1) fileBuffer

        (ls,l) = case Seq.viewr left of
            Seq.EmptyR     -> (def, T.empty)
            (xs Seq.:> xl) ->
                let (x,_) = splitAfter (posColumn $ rangeStart updatedRange) xl
                in (FileBuffer xs, lineToText x)

        (rs,r) = case Seq.viewl right of
            Seq.EmptyL     -> (def, T.empty)
            (yl Seq.:< ys) ->
                let (_,y) = splitAfter (posColumn $ rangeEnd updatedRange) yl
                in (FileBuffer ys, lineToText y)

        m = mconcat [l,updatedValue, r]

--------------------------------------------------------------------------------

-- | Represents an iterator over a text buffer.
data TextStream = TextStream {
    textStreamPosition :: Pos,
    textStreamBuffer   :: TextBuffer
} deriving Show

instance Stream TextStream Char where
    uncons TextStream{..} = do
        -- try to retrieve the line from the buffer
        line <- index textStreamBuffer (posLine textStreamPosition)

        guard (not $ isNull line)

        let
            Pos{..} = textStreamPosition
            (_, r)  = splitAfter (posColumn - 1) line

        case viewl (lineBuffer r) of
            EmptyL -> Nothing
            (x :< xs) -> do
                (y,ys) <- uncons x

                if T.null ys then
                    return (y, TextStream (Pos (posLine + 1) 1) textStreamBuffer)
                else
                    return (y, TextStream (Pos posLine (posColumn + 1)) textStreamBuffer)

--------------------------------------------------------------------------------
