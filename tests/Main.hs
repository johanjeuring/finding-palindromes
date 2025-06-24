{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.

© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring -}
module Main (main) where

import Test.HUnit (Counts, Test (..), runTestTT)
import Test.QuickCheck (quickCheck)

import Data.Algorithms.Palindromes.Finders (Algorithm (..))
import ITApproximate (testListITApproximate)
import ITLinear (testListITLinear)
import ITQuadratic (testListITQuadratic)
import QuickCheckProperties (propertyList)
import UTApproximateAlgorithm (testListApproximateAlgorithm)
import UTDNAPals (testListDNA)
import UTExtendPals (testListExtend)
import UTFinders (testListFinders)
import UTGetLeftRight (testListGetLeftRight)
import UTLinearAlgorithm (testListLinearAlgorithm)
import UTPalEq (testListPalEq)
import UTProcessing (testListProcessing)
import UTPunctuationPals (testListPunctuation)
import UTQuadraticAlgorithm (testListQuadraticAlgorithm)
import UTTextPals (testListText)
import UTWordPals (testListWordPalindromes)

tests :: Test
tests =
    TestList $
        testListLinearAlgorithm
            ++ testListITApproximate
            ++ testListQuadraticAlgorithm
            ++ testListApproximateAlgorithm
            ++ testListText AlgLinear
            ++ testListText AlgQuadratic{algGapSize = 0, algMaxErrors = 0}
            ++ testListText AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            ++ testListPunctuation
            ++ testListGetLeftRight
            ++ testListDNA AlgLinear
            ++ testListDNA AlgQuadratic{algGapSize = 0, algMaxErrors = 0}
            ++ testListText AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            ++ testListExtend AlgLinear
            ++ testListExtend AlgQuadratic{algGapSize = 0, algMaxErrors = 0}
            ++ testListText AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            ++ testListProcessing
            ++ testListPalEq
            ++ testListWordPalindromes AlgLinear
            ++ testListWordPalindromes AlgQuadratic{algGapSize = 0, algMaxErrors = 0}
            ++ testListText AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            ++ testListFinders
            ++ testListITLinear
            ++ testListITQuadratic

runQuickCheck :: IO ()
runQuickCheck = mapM_ quickCheck propertyList

main :: IO Counts
main = do
    runQuickCheck
    runTestTT tests