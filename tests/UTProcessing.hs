{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) -}
module UTProcessing (testListProcessing) where

import Data.Vector (fromList)
import Test.HUnit (Test (..), assertEqual)

import qualified Data.Algorithms.Palindromes.PostProcessing as Post
import qualified Data.Algorithms.Palindromes.PreProcessing as Pre

testListProcessing =
    [ testFilterLetters
    , testTextToWord
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
testFilterPunctuationOnlySpace =
    TestCase $
        assertEqual
            "testFilterPunctuationOnlySpace"
            [(0, 0), (0, 0), (1, 1), (1, 1), (2, 2), (2, 2), (3, 3), (3, 4), (4, 4)]
            ( Post.filterPunctuation
                "aab a"
                [(0, 0), (0, 1), (0, 2), (1, 2), (2, 2), (1, 4), (3, 3), (3, 4), (4, 4)]
            )

testFilterPunctuationStaysSame =
    TestCase $
        assertEqual
            "testFilterPunctuationStaysSame"
            [(0, 0), (0, 1), (1, 1), (0, 3), (2, 2), (2, 3), (3, 3)]
            (Post.filterPunctuation "a b a" [(0, 0), (0, 1), (1, 1), (0, 3), (2, 2), (2, 3), (3, 3)])

testFilterPunctuationPunctuationAndSpaces =
    TestCase $
        assertEqual
            "testFilterPunctuationPunctuationAndSpace"
            [ (0, 0)
            , (0, 1)
            , (1, 1)
            , (1, 2)
            , (2, 2)
            , (2, 2)
            , (0, 6)
            , (3, 3)
            , (4, 4)
            , (4, 4)
            , (5, 5)
            , (5, 6)
            , (6, 6)
            ]
            ( Post.filterPunctuation
                "a.b,aab a "
                [ (0, 0)
                , (0, 1)
                , (1, 1)
                , (0, 3)
                , (2, 2)
                , (2, 3)
                , (0, 6)
                , (3, 4)
                , (4, 4)
                , (3, 6)
                , (5, 5)
                , (5, 6)
                , (6, 6)
                ]
            )
