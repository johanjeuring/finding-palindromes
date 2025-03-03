module UTDNAPals where

import Data.Algorithms.Palindromes.PalindromesUtils (Flag)
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
        assertEqual "palindrome1" "0 to 20\t\"gcgcgcatatatatgcgcgc\"\t20" $
            longestDNAPalindrome t $
                fromList "GCGCGCATATATATGCGCGC"

testDNAPalindrome2 t =
    TestCase $
        assertEqual "palindrome1" "0 to 6\t\"tatata\"\t6" $
            longestDNAPalindrome t $
                fromList "TATATA"
