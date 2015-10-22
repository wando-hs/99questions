module ListsSpec(main, spec) where

import Test.Hspec

import Problem01
import Problem03
import Problem04
import Problem06
import Problem07
import Problem08
import Problem09

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Questions 1 to 10: Lists" $ do
    it "Problem 1: Find the last element of a list" $ do
      mylast [1,2,3,4] `shouldBe` 4
    it "Problem 3: Find the K'th element of a list. The first element in the list is number 1" $ do
      kth [1,2,3] 2 `shouldBe` 2
    it "Problem 4: Find the number of elements of a list" $ do
      myLength [123, 456, 789] `shouldBe` 3
    it "Problem 6: Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x)" $ do
      isPalindrome [1,2,3] `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
    it "Problem 7: Flatten a nested list structure." $ do
      flatten [['a']] `shouldBe` ['a']
    it "Problem 8: Eliminate consecutive duplicates of list elements." $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"
    it "Problem 9: Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists." $ do
      pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe` ["aaaa","b","cc","aa","d","eeee"]
      

      
      