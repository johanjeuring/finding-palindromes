module UTDNAPals where

import Data.ByteString.Internal (w2c)
import Data.Vector (fromList)
import PalindromeMethods (longestDNAPalindrome)
import Test.HUnit (Test (..), assertEqual)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

testListDNA t =
    [ TestLabel "testDNAPalindrome1" $ testDNAPalindrome1 t
    , TestLabel "testDNAPalindrome2" $ testDNAPalindrome2 t
    ]

testDNAPalindrome1 t =
    TestCase $
        assertEqual "palindrome1" "gcgcgcatatatatgcgcgc" $
            longestDNAPalindrome t "gcgcgcatatatatgcgcgc"

testDNAPalindrome2 t =
    TestCase $
        assertEqual "palindrome1" "tatata" $
            longestDNAPalindrome t "tatata"
