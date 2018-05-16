module Git.Index.Ignore where

import Control.Applicative ((<|>))
import Control.Monad (replicateM_)
import Data.Attoparsec.ByteString.Char8
  ( Parser, anyChar, char, eitherP, many', many1, peekChar, satisfy, sepBy1
  , takeTill)
import qualified Data.ByteString.Char8 as Char8
import Data.Monoid ((<>))

import Git.Internal (char_, parses)

import Git.Index.Glob
  ( DirectoryIgnorePattern(..), DoubleStar(..), Glob(..), GlobPart(..)
  , specialChars)

data IgnorePattern
  = Ignore DirectoryIgnorePattern
  | DontIgnore DirectoryIgnorePattern
  deriving (Show)

ipP :: Parser IgnorePattern
ipP = dontIgnoreP <|> ignoreP
  where
    ignoreP = Ignore <$> dipP
    dontIgnoreP = DontIgnore <$> (char '!' >> dipP)

dipP :: Parser DirectoryIgnorePattern
dipP = Dip <$> (globP `sepBy1` char_ '/') <*> parses (char '/')

globP :: Parser (Either DoubleStar Glob)
globP = eitherP doubleStar (Glob <$> many1 globPart)

doubleStar :: Parser DoubleStar
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
    normal = takeTill (\c -> c == '\\' || c `elem` specialChars)
    escaped = do
      c <- char '\\' >> (satisfy (`elem` specialChars) <|> char '\\')
      t <- normal
      return $ Char8.singleton c <> t
