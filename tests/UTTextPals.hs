module UTTextPals where

import Data.Algorithms.Palindromes.PalindromesUtils (Flag)
import Data.Vector (fromList)
import PalindromeMethods (longestTextPalindrome)
import Test.HUnit (Test (..), assertEqual)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

testTextPalindrome1
    , testTextPalindrome2
    , testTextPalindrome3
    , testTextPalindrome4
    , testTextPalindrome5
    , testTextPalindrome6
    , testTextPalindrome7
    , testTextPalindrome8
    , testTextPalindrome9
    , testTextPalindrome10
    , testTextPalindrome11
        :: Flag -> Test

testListText t =
    [ TestLabel "testTextPalindrome10" $ testTextPalindrome10 t
    , TestLabel "testTextPalindrome11" $ testTextPalindrome11 t
    , TestLabel "testTextPalindrome1" $ testTextPalindrome1 t
    , TestLabel "testTextPalindrome2" $ testTextPalindrome2 t
    , TestLabel "testTextPalindrome3" $ testTextPalindrome3 t
    , TestLabel "testTextPalindrome4" $ testTextPalindrome4 t
    , TestLabel "testTextPalindrome5" $ testTextPalindrome5 t
    , TestLabel "testTextPalindrome6" $ testTextPalindrome6 t
    , TestLabel "testTextPalindrome7" $ testTextPalindrome7 t
    , TestLabel "testTextPalindrome8" $ testTextPalindrome8 t
    , TestLabel "testTextPalindrome9" $ testTextPalindrome9 t
    ]

testTextPalindrome1 t =
    TestCase $
        assertEqual "textPalindrome1" "a,ba." $
            longestTextPalindrome t $
                fromList "abcdea,ba."

testTextPalindrome2 t =
    TestCase $
        assertEqual "textPalindrome2" "a,ba" $
            longestTextPalindrome t $
                fromList "abcdea,ba"

testTextPalindrome3 t =
    TestCase $
        assertEqual "textPalindrome3" ".a,ba" $
            longestTextPalindrome t $
                fromList "abcde.a,ba"

testTextPalindrome4 t =
    TestCase $
        assertEqual "textPalindrome4" ".a,ba" $
            longestTextPalindrome t $
                fromList "abcde.a,baf"

testTextPalindrome5 t =
    TestCase $
        assertEqual "textPalindrome5" ".ab,a" $
            longestTextPalindrome t $
                fromList ".ab,acdef"

testTextPalindrome6 t =
    TestCase $
        assertEqual "textPalindrome6" "ab,a" $
            longestTextPalindrome t $
                fromList "ab,acdef"

testTextPalindrome7 t =
    TestCase $
        assertEqual "textPalindrome7" "ab,a." $
            longestTextPalindrome t $
                fromList "ab,a.cdef"

testTextPalindrome8 t =
    TestCase $
        assertEqual "textPalindrome8" ".ab,a." $
            longestTextPalindrome t $
                fromList "g.ab,a.cdef"

testTextPalindrome9 t =
    TestCase $
        assertEqual "textPalindrome9" "" $
            longestTextPalindrome t $
                fromList ""

testTextPalindrome10 t =
    TestCase $ do
        string <- readFile "./examples/palindromes/Damnitimmad.txt"
        assertEqual
            "textPalindrome10"
            ( filter
                (\c -> c /= '\r' && c /= '\\' && c /= '\n')
                string
            )
            $ longestTextPalindrome t (fromList string)

testTextPalindrome11 t =
    TestCase $ do
        string <- readFile "./examples/palindromes/pal17.txt"
        assertEqual
            "textPalindrome11"
            ( filter
                (\c -> c /= '\r' && c /= '\\' && c /= '\n')
                string
            )
            $ longestTextPalindrome t (fromList string)
