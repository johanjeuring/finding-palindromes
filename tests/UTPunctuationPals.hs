module UTPunctuationPals where

import PalindromeMethods (longestPunctuationPalindrome)
import Test.HUnit (Test (..), (~:), (~?=))

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
