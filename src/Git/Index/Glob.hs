module Git.Index.Glob
  ( DirectoryIgnorePattern(..), DoubleStar(..), Glob(..), glob, GlobPart(..)
  , specialChars, star, matchGlob, escape, condenseConsecutive
  , checkPathComponents
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
  ( Parser, parseOnly, anyChar, char, endOfInput, satisfy, string
  , takeByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified System.Path as Path
import System.Path.PartClass (FileDir, AbsRel)
import Text.Printf (printf)

import Git.Internal (SwitchFileDir(..), pathToList)

import Debug.Trace

ts s x = trace (s ++ " " ++ show x) x

data DoubleStar = DoubleStar deriving (Show, Eq)

data DirectoryIgnorePattern
  = Dip
  { dipGlobs :: [Either DoubleStar Glob]
  , dipIsDirectory :: Bool
  } deriving (Show)

mkDip :: [Either DoubleStar Glob] -> Bool -> DirectoryIgnorePattern
mkDip dsogs isDir = Dip (condenseConsecutive (Left DoubleStar) dsogs) isDir

newtype Glob = Glob { unGlob :: [GlobPart] } deriving (Show, Eq)
glob :: [GlobPart] -> Glob
glob = Glob . condenseConsecutive Star

globToString :: Glob -> String
globToString = foldMap globPartToString . unGlob

data GlobPart
  = QuestionMark
  | Star
  | CharRange Char Char
  | Name Char8.ByteString
  deriving (Show, Eq)

specialChars :: [Char]
specialChars = "*?[/"

globPartToString :: GlobPart -> String
globPartToString gp = case gp of
    QuestionMark -> "?"
    Star -> "*"
    CharRange c1 c2 -> printf "[%c-%c]" c1 c2
    Name n -> foldMap escape $ Char8.unpack n

escape :: Char -> String
escape c = if c `elem` specialChars || c == '\\' then '\\' : [c] else [c]

condenseConsecutive :: Eq a => a -> [a] -> [a]
condenseConsecutive x xs = case xs of
    (x0:x1:xs') -> if (x0, x1) == (x, x)
        then condenseConsecutive x $ x1 : xs'
        else x0 : (condenseConsecutive x $ x1 : xs')
    _ -> xs

matchGlobPath
  :: (AbsRel ar, SwitchFileDir fd)
  => DirectoryIgnorePattern -> Path.Path ar fd -> Bool
matchGlobPath dip p = switchFileDir checkFile checkDir checkFileDir p
  where
    checkFile f = not (dipIsDirectory dip) && checkComponents f
    checkDir d = dipIsDirectory dip && checkComponents d
    checkFileDir = checkComponents
    checkComponents :: (AbsRel ar, FileDir fd) => Path.Path ar fd -> Bool
    checkComponents = checkPathComponents (dipGlobs dip) . fmap Char8.pack . pathToList

checkPathComponents :: [Either DoubleStar Glob] -> [Char8.ByteString] -> Bool
checkPathComponents = go
  where
    go [] [] = True
    go [] _ = False
    -- FIXME: I think this pattern need to have [] not _
    go [globOrDs] _ = either (const False) (const True) globOrDs
    go _ [] = False
    go (dsog1:dsog2:dsogs) (comp:comps) = case dsog1 of
      -- We care about matches on the _next_ glob, if available, because then
      -- we can stop matching arbitrarily nested directories:
      Left DoubleStar -> case dsog2 of
        -- The duplicated DoubleStar case _should_ be filtered out anyway:
        Left DoubleStar -> go (dsog2:dsogs) (comp:comps)
        Right (Glob parts) -> case comps of
          [] -> undefined
          (comp2:comps') -> if matchGlob parts comp2 then go dsogs comps else go (dsog2:dsogs) comps'
      Right (Glob parts) -> if matchGlob parts comp then go (dsog2:dsogs) comps else False

matchGlob :: [GlobPart] -> Char8.ByteString -> Bool
matchGlob gps s = either (const False) (const True) $
  parseOnly (matchGlobP gps <* endOfInput) s

matchGlobP :: [GlobPart] -> Parser ()
matchGlobP [] = return ()
matchGlobP (gp:gps) = case gp of
  QuestionMark -> anyChar >> matchGlobP gps
  Star -> case gps of
    [] -> takeByteString >> return ()
    _ -> star $ matchGlobP gps
  CharRange lo hi -> satisfy (\c -> c >= lo && c <= hi) >> matchGlobP gps
  Name n -> string n >> matchGlobP gps

-- | Greedily consume characters until the last time the given parser matches
star :: Parser a -> Parser ()
star nextP = (anyChar >> star nextP) <|> (nextP >> return ())
