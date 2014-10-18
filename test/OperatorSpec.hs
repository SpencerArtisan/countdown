module OperatorSpec(spec) where

import Test.Hspec
import Operator

spec :: Spec
spec = do
   describe "operations" $ do
      it "should return nothing if no values supplied" $ do
          operate (+) [] `shouldBe` 0
         
      it "should return the value if a single value is supplied" $ do
          operate (+) [1] `shouldBe` 1
         
      it "should return the sum of two values for sum" $ do
          operate (+) [1,2] `shouldBe` 3
         
      it "should return the sum of three values for sum" $ do
          operate (+) [1,2,3] `shouldBe` 6

      it "should return the value if a single value is supplied with subtraction" $ do
          operate (-) [1] `shouldBe` 1

      it "should return the difference of two values for subtract" $ do
          operate (-) [1,2] `shouldBe` -1
         
      it "should return the difference of three values for subtract" $ do
          operate (-) [1,2,3] `shouldBe` -4

      it "should evaluate sum and difference of 1 and 2" $ do
          multiOperate [(+), (-)] [1,2] `shouldBe`[3, -1] 
         
      it "should evaluate sum and difference of 1, 2 and 3" $ do
          multiOperate [(+), (-)] [1,2,3] `shouldBe`[6, 0, -4, 2] 
         
      it "should return the difference of three values for subtract" $ do
          operate (-) [1,2,3] `shouldBe` -4
