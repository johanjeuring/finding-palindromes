module UTWordPals where

import Data.Vector (fromList)
import PalindromeMethods (longestWordPalindrome)
import Test.HUnit (Test (..), assertEqual)

testWordPalindrome1
    , testWordPalindrome2
    , testWordPalindrome3
    , testWordPalindrome4
    , testWordPalindrome5
    , testWordPalindrome6
        :: Test

testListWords =
    [ TestLabel "testWordPalindrome1" testWordPalindrome1
    , TestLabel "testWordPalindrome2" testWordPalindrome2
    , TestLabel "testWordPalindrome3" testWordPalindrome3
    , TestLabel "testWordPalindrome4" testWordPalindrome4
    , TestLabel "testWordPalindrome5" testWordPalindrome5
    , TestLabel "testWordPalindrome6" testWordPalindrome6
    , TestLabel "testWordPalindrome7" testWordPalindrome7
    ]

testWordPalindrome1 =
    TestCase $
        assertEqual "wordPalindrome" " is non si, " $
            longestWordPalindrome $
                fromList "what is non si, not?"

testWordPalindrome2 =
    TestCase $
        assertEqual "wordPalindrome" " is non si" $
            longestWordPalindrome $
                fromList "what is non si"

testWordPalindrome3 =
    TestCase $
        assertEqual "wordPalindrome" "is non si, " $
            longestWordPalindrome $
                fromList "is non si, not?"

testWordPalindrome4 =
    TestCase $
        assertEqual "wordPalindrome" "" $
            longestWordPalindrome $
                fromList "aaaaba"

testWordPalindrome5 =
    TestCase $
        assertEqual "wordPalindrome" " a" $
            longestWordPalindrome $
                fromList "aaaab a"

testWordPalindrome6 =
    TestCase $
        assertEqual "wordPalindrome" " waaw " $
            longestWordPalindrome $
                fromList "w waaw wo waw"

testWordPalindrome7 =
    TestCase
        $ assertEqual
            "wordPalindrome"
            " v waaw v"
        $ longestWordPalindrome
        $ fromList "vwaawvxy v waaw v"