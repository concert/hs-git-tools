{-# OPTIONS_GHC -Wno-orphans #-}

module Git.Sha1Spec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Control.Monad (replicateM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.String (IsString)
import qualified Data.Text as Text
import Data.Word

import Git.Internal ()
import Git.Sha1 (Sha1)
import qualified Git.Sha1 as Sha1

instance Arbitrary Sha1 where
  arbitrary = Sha1.unsafeSha1 . BS.pack <$>
    replicateM Sha1.sha1Size (arbitrary @Word8)


spec :: Spec
spec = describe "Sha1" $ do
  describe "hashing" $ do
    it "should produce a Sha1 of the correct size" $ property $
      \bs -> (sha1Len $ Sha1.hashLazy $ LBS.fromStrict bs) == Sha1.sha1Size

  hexSpec "ByteString" Sha1.toHexByteString Sha1.fromHexByteString BS.length
  hexSpec "String" Sha1.toHexString Sha1.fromHexString Prelude.length
  hexSpec "Text" Sha1.toHexText Sha1.fromHexText Text.length


hexSpec
  :: IsString a
  => String -> (Sha1 -> a) -> (a -> Either String Sha1) -> (a -> Int) -> Spec
hexSpec typeString fromSha1 toSha1 len =
  describe (typeString ++ " conversion to Sha1") $ do
    it "should convert a known sha1 string" $
      toSha1 "0123456789abcdef0123456789abcdef01234567" `shouldBe`
        Right (Sha1.unsafeSha1
          "\SOH#Eg\137\171\205\239\SOH#Eg\137\171\205\239\SOH#Eg")
    it "should convert back to string of correct length" $ property $
      \sha1 -> (len $ fromSha1 sha1) == Sha1.sha1HexSize
    it "should roundtrip ok" $ property $ \sha1 -> roundtrip sha1 == Right sha1
    it "should produce sha1 of the correct length" $ property $
      \sha1 -> (sha1Len <$> roundtrip sha1) == Right (sha1Len sha1)
  where
    roundtrip = toSha1 . fromSha1

sha1Len :: Sha1 -> Int
sha1Len = BS.length . Sha1.unSha1
