module OperatorSpec(spec) where

import Test.Hspec
import Operator

spec :: Spec
spec = do
   describe "operations" $ do
      it "should return nothing if no values supplied" $ do
          operateWithMemo [plus,minus] [] `shouldBe` []
         
      it "should return the value if a single value is supplied" $ do
          operateWithMemo [plus,minus] [Solution 1 "1"] `shouldBe` [Solution 1 "1"]
         
      it "should return the sum of two values for sum" $ do
          operateWithMemo [plus,minus] [Solution 1 "1", Solution 2 "2"] `shouldBe` [Solution 3 "1+2",Solution (-1) "1-2"]
         
      it "should return the sum of three values for sum" $ do
          operateWithMemo [plus,minus] [Solution 1 "1",Solution 2 "2",Solution 3 "3"] `shouldBe` [Solution 6 "1+2+3",Solution 0 "1+2-3",Solution 2 "1-2+3",Solution (-4) "1-2-3"]

