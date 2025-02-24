module UTWordPals where

import PalindromeMethods (longestWordPalindrome)
import Test.HUnit (Test (..), assertEqual)

import qualified Data.ByteString.Char8 as BC

testWordPalindrome1
    , testWordPalindrome2
    , testWordPalindrome3
    , testWordPalindrome4
    , testWordPalindrome5
    , testWordPalindrome6 ::
        Test

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
        assertEqual "wordPalindrome" "\" is non si, \"" $
            longestWordPalindrome $
                BC.pack "what is non si, not?"

testWordPalindrome2 =
    TestCase $
        assertEqual "wordPalindrome" "\" is non si\"" $
            longestWordPalindrome $
                BC.pack "what is non si"

testWordPalindrome3 =
    TestCase $
        assertEqual "wordPalindrome" "\"is non si, \"" $
            longestWordPalindrome $
                BC.pack "is non si, not?"

testWordPalindrome4 =
    TestCase $
        assertEqual "wordPalindrome" "" $
            longestWordPalindrome $
                BC.pack "aaaaba"

testWordPalindrome5 =
    TestCase $
        assertEqual "wordPalindrome" "\" a\"" $
            longestWordPalindrome $
                BC.pack "aaaab a"

testWordPalindrome6 =
    TestCase $
        assertEqual "wordPalindrome" "\" waaw \"" $
            longestWordPalindrome $
                BC.pack "w waaw wo waw"

testWordPalindrome7 =
    TestCase $
        assertEqual "wordPalindrome" "\" v waaw v\"" $
            longestWordPalindrome $
                BC.pack "vwaawvxy v waaw v"