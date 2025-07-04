{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.

© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring -}

module UTPalEq (testListPalEq) where

import Data.Maybe (fromJust)
import Test.HUnit (Test (..), (~:), (~?=))

import Data.Algorithms.Palindromes.DNA
    ( DNA (A, C, G, T)
    , toDNA
    )
import Data.Algorithms.Palindromes.PalEq
    ( PalEq ((=:=))
    , palEqToItselfAtIndex
    )

import qualified Data.Vector.Unboxed as U

testListPalEq :: [Test]
testListPalEq =
    testsPalEq
        ++ [ testPalEqToItselfTrue
           , testPalEqToItselfFalse
           , testPalEqToItselfAtIndexReflexive
           , testPalEqToItselfAtIndexAntiReflexive
           , testPalEqToItselfAtIndexOutOfLowerBound
           , testPalEqToItselfAtIndexOutOfUpperBound
           ]

{-
--------------------------------------
  Begin tests for PalEq definition & DNA definition and functions
--------------------------------------
-}
testsPalEq =
    [ "testIntegerPalEq" ~: (5 :: Int) =:= 5 ~?= True
    , "testIntegerInPalEq" ~: (5 :: Int) =:= 3 ~?= False
    , "testCharPalEq" ~: 'a' =:= 'a' ~?= True
    , "testCharInPalEq" ~: 'a' =:= 'b' ~?= False
    , "testDNAPalEq" ~: A =:= T ~?= True
    , "testDNAPalEq2" ~: T =:= A ~?= True
    , "testDNAInPalEq1" ~: A =:= C ~?= False
    , "testDNAInPalEq2" ~: A =:= A ~?= False
    , "testCharToDNA" ~: (U.toList . fromJust . toDNA . U.fromList) "aA" ~?= [A, A]
    ]

{-
--------------------------------------
  End tests for DNA definition and functions
--------------------------------------
-}

{-
----------------------------------------------------------------------
Begin tests for (=:=) and PalEqToItselfAtIndex
----------------------------------------------------------------------
-}

-- | Test with an element that is PalEq with itself.
testPalEqToItselfTrue =
    "testPalEqToItselfTrue"
        ~: 'b'
        =:= 'b'
        ~?= True

-- | Test with an element that is not PalEq to itself.
testPalEqToItselfFalse =
    "testPalEqToItselfFalse"
        ~: A
        =:= A
        ~?= False

-- | Test with a reflexive datatype and index in range.
testPalEqToItselfAtIndexReflexive =
    "testPalEqToItselfAtIndex"
        ~: palEqToItselfAtIndex (U.fromList "abc") 2
        ~?= True

-- | Test with an anti-reflexive datatype and index in range.
testPalEqToItselfAtIndexAntiReflexive =
    "testPalEqToItselfAtIndex"
        ~: palEqToItselfAtIndex (U.fromList [A, T, C]) 2
        ~?= False

-- | Test with an out of range index that is too small.
testPalEqToItselfAtIndexOutOfLowerBound =
    "testPalEqToItselfAtIndexOutOfLowerBound"
        ~: palEqToItselfAtIndex (U.fromList "abc") (-1)
        ~?= False

-- | Test with an out of range index that is too big.
testPalEqToItselfAtIndexOutOfUpperBound =
    "testPalEqToItselfAtIndexOutOfUpperBound"
        ~: palEqToItselfAtIndex (U.fromList "abc") 3
        ~?= False

{-
----------------------------------------------------------------------
End tests for palEqToItself and palEqToItselfAtIndex
----------------------------------------------------------------------
-}
