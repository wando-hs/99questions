module Problem31Spec where

import Test.Hspec
import qualified Problem31

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Check abou primes" $ do
        it "Verify that 41 is prime" $
            Problem31.isPrime 41 `shouldBe` True
        it "Verify that 121 isn't prime" $
            Problem31.isPrime 121 `shouldBe` False