module Data.XML.ClarkSpec
  ( spec
  ) where

import Test.Hspec
-- import Test.QuickCheck
-- import Data.XML.Clark

spec :: Spec
spec = do
  describe "Data.XML.Clark" $ do
    it "does stuff" $ do
      pendingWith "No namespace logic implemented yet, only a data model..."

-- inspiration
--    it "removes leading and trailing whitespace" $ do
--      strip "\t  foo bar\n" `shouldBe` "foo bar"
--    it "is idempotent" $ property $
--      \str -> strip str === strip (strip str)
