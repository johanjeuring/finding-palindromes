module UTPalindromesUtils where

import Data.Algorithms.Palindromes.PalindromesUtils
    ( Couplable ((=:=))
    , DNA (A, C, G, T)
    , couplableWithItselfAtIndex
    , toDNA
    )
import Data.Maybe (fromJust)
import Test.HUnit (Test (..), (~:), (~?=))

import qualified Data.Vector as V

testListPalindromesUtils :: [Test]
testListPalindromesUtils =
    testsCouplable
        ++ [ testCouplableWithItselfTrue
           , testCouplableWithItselfFalse
           , testCouplableWithItselfAtIndexReflexive
           , testCouplableWithItselfAtIndexAntiReflexive
           , testCouplableWithItselfAtIndexOutOfLowerBound
           , testCouplableWithItselfAtIndexOutOfUpperBound
           ]

{-
--------------------------------------
  Begin tests for couplable definition & DNA definition and functions
--------------------------------------
-}
testsCouplable =
    [ "testIntegerCouplable" ~: (5 :: Int) =:= 5 ~?= True
    , "testIntegerIncouplable" ~: (5 :: Int) =:= 3 ~?= False
    , "testCharCouplable" ~: 'a' =:= 'a' ~?= True
    , "testCharIncouplable" ~: 'a' =:= 'b' ~?= False
    , "testDNACouplable" ~: A =:= T ~?= True
    , "testDNACouplable2" ~: T =:= A ~?= True
    , "testDNAIncouplable1" ~: A =:= C ~?= False
    , "testDNAIncouplable2" ~: A =:= A ~?= False
    , "testCharToDNA" ~: (fromJust . toDNA) "aA" ~?= [A, A]
    ]

{-
--------------------------------------
  End tests for DNA definition and functions
--------------------------------------
-}

{-
----------------------------------------------------------------------
Begin tests for (=:=) and couplableWithItselfAtIndex
----------------------------------------------------------------------
-}

-- | Test with an element that is couplable with itself.
testCouplableWithItselfTrue =
    "testCouplableWithItselfTrue"
        ~: 'b'
        =:= 'b'
        ~?= True

-- | Test with an element that is not couplable with itself.
testCouplableWithItselfFalse =
    "testCouplableWithItselfFalse"
        ~: A
        =:= A
        ~?= False

-- | Test with a reflexive datatype and index in range.
testCouplableWithItselfAtIndexReflexive =
    "testCouplableWithItselfAtIndex"
        ~: couplableWithItselfAtIndex (V.fromList "abc") 2
        ~?= True

-- | Test with an anti-reflexive datatype and index in range.
testCouplableWithItselfAtIndexAntiReflexive =
    "testCouplableWithItselfAtIndex"
        ~: couplableWithItselfAtIndex (V.fromList [A, T, C]) 2
        ~?= False

-- | Test with an out of range index that is too small.
testCouplableWithItselfAtIndexOutOfLowerBound =
    "testCouplableWithItselfAtIndexOutOfLowerBound"
        ~: couplableWithItselfAtIndex (V.fromList "abc") (-1)
        ~?= False

-- | Test with an out of range index that is too big.
testCouplableWithItselfAtIndexOutOfUpperBound =
    "testCouplableWithItselfAtIndexOutOfUpperBound"
        ~: couplableWithItselfAtIndex (V.fromList "abc") 3
        ~?= False

{-
----------------------------------------------------------------------
End tests for couplableWithItself and couplableWithItselfAtIndex
----------------------------------------------------------------------
-}
