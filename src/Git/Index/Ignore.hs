module Git.Index.Ignore where

import Control.Applicative ((<|>))
import Control.Monad (replicateM_)
import Data.Attoparsec.ByteString.Char8
  (Parser, anyChar, char, many', many1, peekChar, satisfy, sepBy1, takeTill)
import qualified Data.ByteString.Char8 as Char8
import Data.Monoid ((<>))

import Git.Internal (char_, parses)

data IgnorePattern
  = Ignore DirectoryIgnorePattern
  | DontIgnore DirectoryIgnorePattern
  deriving (Show)

data DirectoryIgnorePattern
  = Dip
  { dipGlobs :: [Glob]
  , dipIsDirectory :: Bool
  } deriving (Show)

data Glob
  = DoubleStar
  | Glob [GlobPart]
  deriving (Show)

data GlobPart
  = QuestionMark
  | Star
  | CharRange Char Char
  | Name Char8.ByteString
  deriving (Show)


ipP :: Parser IgnorePattern
ipP = dontIgnoreP <|> ignoreP
  where
    ignoreP = Ignore <$> dipP
    dontIgnoreP = DontIgnore <$> (char '!' >> dipP)

dipP :: Parser DirectoryIgnorePattern
dipP = Dip <$> (globP `sepBy1` char_ '/') <*> parses (char '/')

globP :: Parser Glob
globP = doubleStar <|> (Glob <$> many1 globPart)

doubleStar :: Parser Glob
doubleStar = replicateM_ 2 (char '*') >> return DoubleStar

globPart :: Parser GlobPart
globPart = name <|> star <|> qm <|> charRange

qm :: Parser GlobPart
qm = char '?' >> return QuestionMark

star :: Parser GlobPart
star = do
  char_ '*'
  c <- peekChar
  case c of
    Just '*' -> fail "Glob: unexpected consecutive chars"
    _ -> return Star

charRange :: Parser GlobPart
charRange = do
  char_ '['
  lo <- anyChar
  char_ '-'
  hi <- anyChar
  return $ CharRange lo hi

special :: [Char]
special = "*?[/"

name :: Parser GlobPart
name = do
  n <- takeEscaped
  if Char8.null n then fail "empty name" else return $ Name n

-- Thanks to https://stackoverflow.com/questions/35300812/fast-parsing-of-\
-- string-that-allows-escaped-characters
takeEscaped :: Parser Char8.ByteString
takeEscaped = do
    t <- normal
    ts <- many' escaped
    return $ t <> mconcat ts
  where
    normal = takeTill (\c -> c == '\\' || c `elem` special)
    escaped = do
      c <- char '\\' >> (satisfy (`elem` special) <|> char '\\')
      t <- normal
      return $ Char8.singleton c <> t
