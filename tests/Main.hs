-----------------------------------------------------------------------------
--
-- Module      :  tests.Main
-- Copyright   :  (c) 2007 - 2013 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Main where

import Data.Algorithms.Palindromes.Combinators (Complexity (..))
import ITLinear (testListITLinear)
import ITQuadratic (testListITQuadratic)
import PalindromeProperties (propTextPalindrome)
import Test.HUnit (Counts, Test (..), runTestTT)
import Test.QuickCheck (quickCheck)
import UTCombinators
    ( propValidPalindromeRangeAndText
    , propValidPalindromeRangeAndTextPlain
    , propValidPalindromeRangeAndTextText
    , propValidPalindromeRangeAndTextWord
    , testListCombinators
    )
import UTDNAPals (testListDNA)
import UTExtendPals (testListExtend)
import UTGetLeftRight (testListGetLeftRight)
import UTPalindromesUtils (testListPalindromesUtils)
import UTProcessing (testListProcessing)
import UTPunctuationPals (testListPunctuation)
import UTQuadraticAlgorithm (testListQuadraticAlgorithm)
import UTTextPals (testListText)
import UTWordPals (testListWordPalindromes)

tests :: Test
tests =
    TestList $
        testListText ComLinear
            ++ testListText ComQuadratic{gapSize = 0, maxError = 0}
            ++ testListPunctuation
            ++ testListGetLeftRight
            ++ testListDNA ComQuadratic{gapSize = 0, maxError = 0}
            ++ testListQuadraticAlgorithm
            ++ testListExtend ComLinear
            ++ testListExtend ComQuadratic{gapSize = 0, maxError = 0}
            ++ testListProcessing
            ++ testListPalindromesUtils
            ++ testListWordPalindromes ComLinear
            ++ testListCombinators
            ++ testListITLinear
            ++ testListITQuadratic

main :: IO Counts
main = do
    -- quickCheck propPalindromesAroundCentres
    quickCheck propTextPalindrome
    quickCheck propValidPalindromeRangeAndTextPlain
    quickCheck propValidPalindromeRangeAndTextText
    quickCheck propValidPalindromeRangeAndTextWord
    runTestTT tests

{-
2nd property falsified by
"m\159:t\231\202\r\STX-me\230\&9JS/\EM'5\164\171\148\&5A@\196\242\f\157jY\NULB,\134\179\ESCS`:\ff\203\b\130\&0\DC3Yni>L"

"\GS[\242\tx\ENQ3\247\&3\130(\NUL?zX\215\DC3"

"\213\SI6+\ESCU:1\165\254\228\SUB9\200\231\USM,3\227\&3\176\214X\203\SOH\130UI9\154\239<w\231kPbmvY|!sc\133\b$#v\203LM\235H"

\*** Failed! Falsified (after 82 tests):
"# 7z;K\EM\996701\f\EOT\1088669\RSy\30490\EM\149585h\ENQ\r\1093014a\r6\181326\SI\DC2\b\n\61832\\\DC2+\11953\61349\\\STX>a\133690\145589W\996131\136065}-6cJI[\CAN\DC1\997287T\92545"

-}

{-

-- Code for benchmarking. Needs to go in a separate file.

import Criterion.Main
import Data.Algorithms.Palindromes.Palindromes
import System.IO
import qualified Data.ByteString as B

main :: IO ()
main =
  do fnenhl <- openFile  "examples/palindromes/Damnitimmad.txt" ReadMode-- "../../TestSources/Bibles/engelskingjames.txt" ReadMode
     hSetEncoding fnenhl latin1
     inputenB <- B.hGetContents fnenhl
--     fnnlhl <- openFile  "../../TestSources/Bibles/nederlands.txt" ReadMode
--     hSetEncoding fnnlhl latin1
--    inputnlB <- B.hGetContents fnnlhl
     defaultMain
       [
--        bench "longestPalindromeConstantArguments Dutch" (nf CA.longestPalindrome inputnlB)--,
--        bench "longestPalindrome Dutch" (nf longestPalindrome inputnlB)--,
--       bench "longestPalindromeConstantArguments English" (nf CA.longestPalindrome inputenB)--,
        bench "longestPalindrome English" (nf longestPalindrome inputenB)--,--,
       ]

-}
{-

To compare my solution and Rampion's lazy solution:

       [bench "lengthLongestPalindromes" (nf (palindromesAroundCentres (==) . listArrayl0) input)
       ,bench "Rampion's solution" (nf maximalPalindromeLengths input)
       ]

-}
