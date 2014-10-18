module CountdownSpec(spec) where

import Test.Hspec
import Countdown

spec :: Spec
spec = do
   describe "countdown results" $ do
      it "should find no results from the empty set" $ do
          countdown [] `shouldBe` []

      it "should find one result from a single value" $ do
          countdown [1] `shouldBe` [(1,"1")]

      it "should find multiple results from two values" $ do
          countdown [1,2] `shouldBe` [(1,"1"), (2,"2"), (3,"1+2"), (-1,"1-2"), (3,"2+1"), (1,"2-1")] 

