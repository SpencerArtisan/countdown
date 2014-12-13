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
          countdown [1,2] `shouldBe` [Solution 3 "1+2", Solution (-1) "1-2", Solution 2 "1*2", Solution 1 "1", Solution 3 "2+1", Solution 1 "2-1", Solution 2 "2*1", Solution 2 "2/1", Solution 1 "1", Solution 2 "2"] 

      it "might fail to find a calculation to match a target" $ do
          countdownTarget [1,2] 99 `shouldBe` "No answer"

      it "should find a calculation to match a target" $ do
          countdownTarget [25,7,5,1,9,5] 241 `shouldBe` "(25+5+1-5)*9+7"

      it "should find calculations to match a target" $ do
          countdownTargets [3,6,25,50,75,100] 952 `shouldBe` ["(((3+100)*6)*75)/50+25","(((3+100)*75)*6)/50+25","(((6+100)*3)*75-50)/25","(((6+100)*75)*3-50)/25","(((100+3)*6)*75)/50+25","(((100+3)*75)*6)/50+25","(((100+6)*3)*75-50)/25","(((100+6)*75)*3-50)/25"]


