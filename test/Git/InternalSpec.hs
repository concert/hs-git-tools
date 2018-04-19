{-# OPTIONS_GHC -Wno-orphans #-}

module Git.InternalSpec where

import Test.Hspec

import Data.Bits (Bits)
import Data.Typeable (Typeable, typeOf, Proxy(..))
import Data.Word

import Git.Internal (lowMask)

spec :: Spec
spec = describe "Internal" $ do
  describe "lowMask" $ do
    lowMaskSpec $ Proxy @Int
    lowMaskSpec $ Proxy @Word16

lowMaskSpec
  :: forall a. (Typeable a, Num a, Show a, Bits a, Bounded a) => Proxy a -> Spec
lowMaskSpec _ = let name = show $ typeOf $ minBound @a in do
  it ("should leave a small " ++ name ++ " untouched") $
    mapM_ (shouldBe (3 :: a) . lowMask 3) [2, 3, 4]
  it ("should trunctate a large " ++ name) $
    lowMask 15 3 `shouldBe` (7 :: a)
