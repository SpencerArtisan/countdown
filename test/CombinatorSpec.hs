module CombinatorSpec(spec) where

import Test.Hspec
import Combinator

spec :: Spec
spec = do
   describe "combinations" $ do
      it "should have no combinations of an empty list" $ do
          combinations [] `shouldBe` []

      it "should have one combination of a singleton list" $ do
          combinations [1] `shouldBe` [[1]]

      it "should have two combinations of a two element list" $ do
          combinations [1,2] `shouldBe` [[1,2], [2,1]]

      it "should have six combinations of a three element list" $ do
          combinations [1,2,3] `shouldBe` [[1,2,3],[2,1,3],[1,3,2],[2,3,1],[3,1,2],[3,2,1]]

