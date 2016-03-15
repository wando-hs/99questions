module Problem28Spec where

import Test.Hspec
import qualified Problem28

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "In item a" $ do
        it "Sort list of lists of int" $
            Problem28.lsort [[1..4], [1,2], [1..4], [1..9], [20]] `shouldBe` [[20], [1,2], [1..4], [1..4], [1..9]]
        it "Sort list of lists of char" $
            Problem28.lsort ["abc","de","fgh","de","ijkl","mn","o"] `shouldBe` ["o","de","de","mn","abc","fgh","ijkl"]