module UTPalindromesUtils where

import Data.Algorithms.Palindromes.PalindromesUtils
    ( Couplable ((=:=))
    , DNA (A, C, G, T)
    , appendseq
    , listArrayl0
    , myIsLetterC
    , surroundedByPunctuation
    , toDNA
    , vecToArray
    )
import Data.Array (listArray)
import Test.HUnit (Test (..), assertEqual, (~:), (~?=))

import qualified Data.Sequence as S
import qualified Data.Vector as V

testListPalindromesUtils :: [Test]
testListPalindromesUtils =
    [ testLineEndIsLetterChar
    , testCharIsLetterChar
    , testControlIsLetterChar
    , testSpaceIsLetterChar
    , testPuncSurroundedByPunctuation
    , testPalNoPunccSurroundedByPunctuation
    , testOutOfBounds1SurroundedByPunctuation
    , testOutOfBounds2SurroundedByPunctuation
    , testOutOfBounds3SurroundedByPunctuation
    , testAppendSeq
    , testlistArrayl0
    , testVecToArray
    ]
        ++ testsCouplable

{-
-------------------------------------
  Begin test to show single palindromes
-------------------------------------
-}
{-
-------------------------------------
  End tests to show single palindromes
-------------------------------------
-}

{-
--------------------------------------
  Begin tests for punctuation utility functions
--------------------------------------
-}

{-
tests for myIsLetterC function.
Tests against the 4 options, a 'normal' char, a isPunctiation char, a IsControl char and a isSpace char
-}
testCharIsLetterChar =
    TestCase
        $ assertEqual
            "testCharIsLetterChar"
            True
        $ myIsLetterC
            'a'

testLineEndIsLetterChar =
    TestCase
        $ assertEqual
            "testLineEndIsLetterChar"
            False
        $ myIsLetterC
            '\n'

-- test for the unicode NUL character
testControlIsLetterChar =
    TestCase
        $ assertEqual
            "testControlIsLetterChar"
            False
        $ myIsLetterC
            '\x00'

testSpaceIsLetterChar =
    TestCase
        $ assertEqual
            "testSpaceIsLetterChar"
            False
        $ myIsLetterC
            ' '

{-
tests for surroundedByPunctuation function.
-}
testPuncSurroundedByPunctuation =
    TestCase
        $ assertEqual
            "testPuncSurroundedByPunctuation"
            True
        $ surroundedByPunctuation
            1
            2
            (V.fromList ".h.")

testPalNoPunccSurroundedByPunctuation =
    TestCase
        $ assertEqual
            "testPalButNoPunccSurroundedByPunctuation"
            False
        $ surroundedByPunctuation
            1
            3
            (V.fromList "aha")

testOutOfBounds1SurroundedByPunctuation =
    TestCase
        $ assertEqual
            "testOutOfBounds1SurroundedByPunctuation"
            True
        $ surroundedByPunctuation
            1
            10
            (V.fromList ".ha")

testOutOfBounds2SurroundedByPunctuation =
    TestCase
        $ assertEqual
            "testOutOfBounds2SurroundedByPunctuation"
            True
        $ surroundedByPunctuation
            (-10)
            2
            (V.fromList "ah.")

testOutOfBounds3SurroundedByPunctuation =
    TestCase
        $ assertEqual
            "testOutOfBounds2SurroundedByPunctuation"
            True
        $ surroundedByPunctuation
            (-10)
            10
            (V.fromList "aha")

{-
--------------------------------------
  End tests for punctuation utility functions
--------------------------------------
-}

{-
----------------------------------------------------
  Begin tests for foldable (Seq/Array/List) utility functions
----------------------------------------------------
-}

testAppendSeq =
    TestCase
        $ assertEqual
            "testAppendSeq"
            "ba"
        $ appendseq
            ("a", S.fromList "b")

testlistArrayl0 =
    TestCase
        $ assertEqual
            "listArrayl0"
            (listArray (0, 1) "ab")
        $ listArrayl0
            "ab"

testVecToArray = "vecToArray" ~: vecToArray (V.fromList "ab") ~?= listArray (0, 1) "ab"

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
    , "testCharToDNA" ~: toDNA "aA" ~?= [A, A]
    ]

{-
--------------------------------------
  End tests for DNA definition and functions
--------------------------------------
-}
