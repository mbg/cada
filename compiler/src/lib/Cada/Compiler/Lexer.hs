--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Lexer where

--------------------------------------------------------------------------------

import Control.Monad

import qualified Data.Char as C
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Combinator
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim

import Cada.Compiler.Lexer.Pos
import Cada.Compiler.Lexer.Token

--------------------------------------------------------------------------------

-- | Enumerates different states the lexer could be in.
data LexState
    -- | The lexer's default and initial state.
    = LexDefault

--------------------------------------------------------------------------------

-- | Enumerates ASCII control characters.
controlCharacters :: String
controlCharacters = ['\t', '\n', '\r', '\f', '\v']

-- | Matches ASCII or Unicode space characters, but not control characters.
isSpace :: MonadParsec s m Char => m Char
isSpace = satisfy $ \c -> C.isSpace c && notElem c controlCharacters

convertPos :: SourcePos -> Pos
convertPos pos = Pos {
    posLine   = sourceLine pos,
    posColumn = sourceColumn pos
}

makeTokenWith :: MonadParsec s m Char => m TokenClass -> m Token
makeTokenWith f = do
    pos <- getPosition
    r <- f
    return $ Tkn r (convertPos pos)

makeToken :: MonadParsec s m Char => TokenClass -> m Token
makeToken tkn = do
    pos <- getPosition
    return $ Tkn tkn (convertPos pos)

--------------------------------------------------------------------------------

-- | `spaceP` parses a space token.
spaceP :: MonadParsec s m Char => m Token
spaceP = makeTokenWith $ do
    xs <- some isSpace
    return $ Whitespace $ Space (length xs)

-- | `tabP` parses a tab token.
tabP :: MonadParsec s m Char => m Token
tabP = makeTokenWith (void tab >> return (Whitespace Tab))

-- | `crlfP` parses a new line token (Windows style line endings)
crlfP :: MonadParsec s m Char => m Token
crlfP = void crlf >> makeToken (Whitespace $ NewLine True)

-- | `nlP` parses a new line token (unix style line endings).
nlP :: MonadParsec s m Char => m Token
nlP = void newline >> makeToken (Whitespace $ NewLine False)

-- | `eofP` parses an end-of-file token.
eofP :: MonadParsec s m Char => m Token
eofP = eof >> makeToken (Whitespace EoF)

-- | `whitespaceP` parses whitespace tokens.
whitespaceP :: MonadParsec s m Char => m Token
whitespaceP = spaceP <|> tabP <|> crlfP <|> nlP <?> "whitespace"

fragmentP :: MonadParsec s m Char => m Token
fragmentP = makeTokenWith $ anyChar >>= \xs -> return (Fragment xs)

--------------------------------------------------------------------------------

--integer :: MonadParsec s m Input => m Token
--integer = Tkn . TInt <$> lexeme L.integer

tkn :: MonadParsec s m Char => m Token
tkn = whitespaceP <|> fragmentP

lexer :: MonadParsec s m Char => m [Token]
lexer = do
    ts <- many tkn
    t  <- eofP
    return (ts ++ [t]) -- TODO: fix this mess

runLexer :: Stream s Char => String -> s -> Either ParseError [Token]
runLexer = runParser (setTabWidth 4 >> lexer)

--------------------------------------------------------------------------------
