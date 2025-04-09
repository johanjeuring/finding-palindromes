{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) -}
module UTProcessing where

import Data.Vector (fromList)
import Test.HUnit (Test (..), assertEqual)

import qualified Data.Algorithms.Palindromes.PostProcessing as Post
import qualified Data.Algorithms.Palindromes.PreProcessing as Pre

testListProcessing =
    [ testFilterLetters
    , testTextToWord
    , testFilterMin
    , testFilterExact
    , testFilterMax
    , testFilterPunctuationOnlySpace
    , testFilterPunctuationStaysSame
    , testFilterPunctuationPunctuationAndSpaces
    ]

-- Preprocessing functions
testFilterLetters =
    TestCase $
        assertEqual
            "testFilterLetters"
            (fromList [(0, 'a'), (2, 'b'), (3, 'a'), (5, 'a')])
            (Pre.filterLetters' "a'ba a")

-- ToDNA test al geschreven in UTPalindromeUtils. Moet verplaatst hiernaartoe

testTextToWord =
    TestCase $
        assertEqual
            "testTextToWord"
            (fromList ["this", "is", "a", "list", "of", "words"])
            (Pre.textToWords "This is a   list of  words. ")

-- Postprocessing functions
testFilterMin =
    TestCase $
        assertEqual
            "testFilterMin"
            [0, 2, 3, 0, 5]
            (Post.filterMin 2 [1, 2, 3, 0, 5])

testFilterExact =
    TestCase $
        assertEqual
            "testFilterExact"
            [0, 2, 0, 0, 0]
            (Post.filterExact 2 [1, 2, 3, 0, 5])
testFilterMax =
    TestCase $
        assertEqual
            "testFilterMax"
            [1, 2, 0, 0, 0]
            (Post.filterMax 2 [1, 2, 3, 0, 5])
testFilterPunctuationOnlySpace =
    TestCase $
        assertEqual
            "testFilterPunctuationOnlySpace"
            [0, 0, 0, 0, 0, 0, 0, 1, 0]
            (Post.filterPunctuation "aab a" [0, 1, 2, 1, 0, 3, 0, 1, 0])

testFilterPunctuationStaysSame =
    TestCase $
        assertEqual
            "testFilterPunctuationStaysSame"
            [0, 1, 0, 3, 0, 1, 0]
            (Post.filterPunctuation "a b a" [0, 1, 0, 3, 0, 1, 0])

testFilterPunctuationPunctuationAndSpaces =
    TestCase $
        assertEqual
            "testFilterPunctuationPunctuationAndSpace"
            [0, 1, 0, 1, 0, 0, 6, 0, 0, 0, 0, 1, 0]
            (Post.filterPunctuation "a.b,aab a " [0, 1, 0, 3, 0, 1, 6, 1, 0, 3, 0, 1, 0])
