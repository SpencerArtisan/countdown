module EchoSpec(spec) where

import Test.Hspec
import Echo

spec :: Spec
spec = do
   describe "echo" $ do
      it "should return what you pass in" $ do
         echo 42 `shouldBe` 42
