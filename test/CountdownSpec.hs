module CountdownSpec(spec) where

import Test.Hspec
import Countdown
import Operator

spec :: Spec
spec = do
   describe "countdown results" $ do
      it "should find no results from the empty set" $ do
          countdown [] `shouldBe` []

      it "should find one result from a single value" $ do
          countdown [1] `shouldBe` [Solution 1 "1"]

      it "should find multiple results from two values" $ do
          countdown [1,2] `shouldBe` [Solution 1 "1", Solution 2 "2", Solution 3 "1+2", Solution (-1) "1-2", Solution 2 "1*2", Solution 3 "2+1", Solution 1 "2-1", Solution 2 "2*1"] 

      it "might fail to find a calculation to match a target" $ do
          countdownTarget [1,2] 99 `shouldBe` "No answer"

      it "should find a calculation to match a target" $ do
          countdownTarget [25,7,5,1,9,5] 241 `shouldBe` "(5*5+1)*9+7"


