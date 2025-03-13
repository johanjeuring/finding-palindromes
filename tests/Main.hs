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

import PalindromeProperties (propPalindromesAroundCentres, propTextPalindrome)
import Test.HUnit (Counts, Test (..), runTestTT)
import Test.QuickCheck (quickCheck)
import UTDNAPals (testListDNA)
import UTExtendPals (testListExtend)
import UTGetLeftRight (testListGetLeftRight)
import UTLinearAlgorithm (testListLinearAlgorithm)
import UTPalindromesUtils (testListPalindromesUtils)
import UTQuadraticAlgorithm (testListQuadraticAlgorithm)
import UTTextPals (testListText)
import UTWordPals (testListWords)

import qualified Data.Algorithms.Palindromes.PalindromesUtils as PU

tests :: Test
tests =
    TestList $
        testListText PU.Linear
            ++ testListText PU.Quadratic
            ++ testListWords
            ++ testListGetLeftRight
            ++ testListDNA PU.Quadratic
            ++ testListQuadraticAlgorithm
            ++ testListExtend PU.Linear
            ++ testListExtend PU.Quadratic
            ++ testListLinearAlgorithm
            ++ testListPalindromesUtils

main :: IO Counts
main = do
    quickCheck propPalindromesAroundCentres
    quickCheck propTextPalindrome
    runTestTT tests

{-
2nd property falsified by
"m\159:t\231\202\r\STX-me\230\&9JS/\EM'5\164\171\148\&5A@\196\242\f\157jY\NULB,\134\179\ESCS`:\ff\203\b\130\&0\DC3Yni>L"

"\GS[\242\tx\ENQ3\247\&3\130(\NUL?zX\215\DC3"

"\213\SI6+\ESCU:1\165\254\228\SUB9\200\231\USM,3\227\&3\176\214X\203\SOH\130UI9\154\239<w\231kPbmvY|!sc\133\b$#v\203LM\235H"

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
