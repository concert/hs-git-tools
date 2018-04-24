module Git.Index.Builder where

import Blaze.ByteString.Builder
  ( Builder, fromByteString, fromWord16be, fromWord32be, toByteString
  , toLazyByteString)
import Crypto.Hash.SHA1 as SHA1
import Data.Bits (Bits, (.|.), shiftR, bit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Clock.System (utcToSystemTime, SystemTime(..))
import Foreign.Marshal.Utils (fromBool)
import qualified System.Path as Path

import Git.Internal (lowMask, assembleBits)
import Git.Sha1 (Sha1(..))
import Git.Types (fileModeToInt)
import Git.Index.Extensions.Class (extensionB)
import Git.Index.Index (Index(..), numEntries)
import Git.Index.Types
  ( IndexVersion(..), versionToWord32, Stage, Stages, stagesToMap, stageToInt
  , IndexEntry(..), GitFileStat(..), Flag(..))


writeIndex :: Path.AbsFile -> Index -> IO ()
writeIndex path idx = undefined

indexLbs :: Index -> LBS.ByteString
indexLbs idx = let lbs = toLazyByteString $ indexB idx in
  lbs <> LBS.fromStrict (SHA1.hashlazy lbs)

indexB :: Index -> Builder
indexB idx@(Index v es ct ru) =
       fromByteString "DIRC" <> fromWord32be (versionToWord32 v)
    <> fromWord32be (numEntries idx) <> entriesBuilder
    <> extensionB ct <> extensionB ru
  where
    entriesBuilder = snd $
      Map.foldlWithKey' sieB' ("", mempty) $
      -- We convert the Paths to Strings so that the encoding (esp V4) can just
      -- worry about serialising the Strings, rather than normalising the Paths:
      Map.mapKeys Path.toString es
    sieB' (prevPath, builder) path stages =
      let (prevPath', builder') = sieB v prevPath path stages in
      (prevPath', builder <> builder')

sieB
  :: IndexVersion -> String -> String -> Stages IndexEntry -> (String, Builder)
sieB v prevPath path stages = Map.foldlWithKey' f (prevPath, mempty) $ stagesToMap stages
  where
    f :: (String, Builder) -> Stage -> IndexEntry -> (String, Builder)
    f (pp, builder) stage (IndexEntry gfs sha1 flags) =
      let
        pathBs = case v of
          Version4 -> encodePathV4 pp path
          _ -> Char8.pack path
        builder' = pad $ gfsB gfs <> fromByteString (unSha1 sha1)
          <> flagsB (BS.length pathBs) stage flags
          <> fromByteString pathBs
      in
        (path, builder <> builder')
    pad = case v of
      Version4 -> id
      _ -> padTo8

padTo8 :: Builder -> Builder
padTo8 b = fromByteString bs <> padding
  where
    bs = toByteString b
    padding = fromByteString $ Char8.pack $
      replicate (8 - (BS.length bs `rem` 8)) '\NUL'

gfsB :: GitFileStat -> Builder
gfsB (GitFileStat mdca ca did ino mode uid gid size) =
      posixTimeB mdca <> posixTimeB ca
   <> f did <> f ino <> f (fileModeToInt mode)
   <> f uid <> f gid <> fromWord32be size
  where
    f :: Integral a => a -> Builder
    f = fromWord32be . fromIntegral

posixTimeB :: POSIXTime -> Builder
posixTimeB pt = fromWord32be seconds <> fromWord32be nanofrac
  where
    st = utcToSystemTime $ posixSecondsToUTCTime pt
    seconds = fromIntegral $ systemSeconds st
    nanofrac = systemNanoseconds st

flagsB :: Int -> Stage -> Set Flag -> Builder
flagsB pathLen stage flags = fromWord16be bits <> continuation
  where
    (!) = Set.member
    shouldContinue = SkipWorkTree ! flags || IntentToAdd ! flags
    continuation = if shouldContinue then fromWord16be moreBits else mempty
    bits = assembleBits
      [ (1, fromBool $ AssumeValid ! flags)
      , (1, fromBool shouldContinue)
      , (2, fromIntegral $ stageToInt stage)
      , (12, fromIntegral $ if pathLen < 0xfff then pathLen else 0xfff)
      ]
    moreBits = assembleBits
      [ (1, 0)
      , (1, fromBool $ SkipWorkTree ! flags)
      , (1, fromBool $ IntentToAdd ! flags)
      , (13, 0)
      ]

encodePathV4 :: String -> String -> BS.ByteString
encodePathV4 s1 s2 = case Text.commonPrefixes t1 t2 of
    Nothing -> f "" t2
    Just (common, _, x) -> f common x
  where
    t1 = Text.pack s1
    t2 = Text.pack s2
    f common newSuffix = chunkNumBs (Text.length t1 - Text.length common)
      <> Char8.pack (Text.unpack newSuffix) <> Char8.singleton '\NUL'

chunkNumBs :: (Integral a, Bits a) => a -> BS.ByteString
chunkNumBs = inner False
  where
    inner prevCont n =
      let
        rest = shiftR n 7
        bits = fromIntegral $ lowMask n 7
        continue = rest > 0
        byte = if prevCont then bit 7 .|. bits else bits
      in
        (if continue then inner True (rest - 1) else "") <> BS.singleton byte
