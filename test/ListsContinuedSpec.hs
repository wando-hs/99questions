module ListsContinuedSpec(main, spec) where

import Test.Hspec

import Problem11

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   describe "Questions 10 to 11: Lists Continued" $ do
     it "Problem 11: Modified run-length encoding" $ do
        encode' "aaaabccaadeeee" `shouldBe` ([Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'] :: [Item])