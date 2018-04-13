module Git.Pack.DeltaSpec where

import Test.Hspec

import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (runIdentity)

import Git.Objects (ObjectType(..))
import Git.Types (GitError(..))
import Git.Pack.Delta
  ( DeltaInstruction(..), DeltaBody(..), deltaBodyOk, applyDelta
  , PackObjectChain(..), renderPackObjectChain)

spec :: Spec
spec = do
  describe "Pack.Delta" $ do
    describe "deltaBodyOk" $ do
      it "should note mismatch of instruction output lens vs recorded len" $ do
        deltaBodyOk (DeltaBody 0 12 [Copy 0 5, Insert "abcdefg"])
          `shouldBe` True
        deltaBodyOk (DeltaBody 0 12 [Copy 0 4, Insert "abcdefg"])
          `shouldBe` False
        deltaBodyOk (DeltaBody 0 12 [Copy 0 5, Insert "abcdef"])
          `shouldBe` False
    describe "applyDelta" $ do
      it "should execute a series of instructions correctly" $
        let
          base = "hello world cruft bye!"
          db = DeltaBody 22 66
            [ Copy 0 6, Insert "is a greeting. The ", Copy 6 6
            , Insert "is where we live. ", Copy 0 11, Insert ", ", Copy 18 4]
        in
          runIdentity (runExceptT $ applyDelta base db) `shouldBe` Right
          "hello is a greeting. The world is where we live. hello world, bye!"

      it "should refuse to act on base data of incorrect length" $
        runIdentity (runExceptT $ applyDelta "hello" $ DeltaBody 3 0 [])
        `shouldBe` Left (FailedPreDeltaApplicationLengthCheck 5 3)

      it "should notice a resulting string of incorrect length" $
        runIdentity (runExceptT $ applyDelta "hello" $ DeltaBody 5 1 [Copy 6 1])
        `shouldBe` Left (FailedPostDeltaApplicationLengthCheck 0 1)

    describe "renderPackObjectData" $ do
      it "should process in the correct order" $
        let
          poc = PackObjectChain ObjTyBlob "hello world"
            [ DeltaBody 11 15 [Insert "greetings", Copy 5 6]
            , DeltaBody 15 16 [Copy 0 10, Insert "planet"]]
        in
          runIdentity (runExceptT $ renderPackObjectChain poc)
          `shouldBe` Right (ObjTyBlob, "greetings planet")
