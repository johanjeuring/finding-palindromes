{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) -}

module UTPunctuationPals (testListPunctuation) where

import Test.HUnit (Test (..), (~:), (~?=))

import PalindromeMethods (longestPunctuationPalindrome)

testListPunctuation :: [Test]
testListPunctuation =
    [ testPunctuationPalindrome1
    , testPunctuationPalindrome2
    , testPunctuationPalindrome3
    , testPunctuationPalindrome4
    , testPunctuationPalindrome5
    , testPunctuationPalindrome6
    , testPunctuationPalindrome7
    ]

testPunctuationPalindrome1 =
    "testPunctuationPalindrome1"
        ~: longestPunctuationPalindrome "what is non si, not?"
        ~?= "is non si"
testPunctuationPalindrome2 =
    "testPunctuationPalindrome2"
        ~: longestPunctuationPalindrome "what is non si"
        ~?= "is non si"
testPunctuationPalindrome3 =
    "testPunctuationPalindrome3"
        ~: longestPunctuationPalindrome "is non si, not?"
        ~?= "is non si"
testPunctuationPalindrome4 =
    "testPunctuationPalindrome4"
        ~: longestPunctuationPalindrome "aaaaba"
        ~?= ""
testPunctuationPalindrome5 =
    "testPunctuationPalindrome5"
        ~: longestPunctuationPalindrome "aaaab a"
        ~?= "a"
testPunctuationPalindrome6 =
    "testPunctuationPalindrome6"
        ~: longestPunctuationPalindrome "w waaw wo waw"
        ~?= "waaw"
testPunctuationPalindrome7 =
    "tesPunctuationPalindrome7"
        ~: longestPunctuationPalindrome "vwaawvxy v waaw v"
        ~?= "v waaw v"
