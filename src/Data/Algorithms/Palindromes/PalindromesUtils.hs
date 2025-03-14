-----------------------------------------------------------------------------
--
-- Module      :  Data.Algorithms.Palindromes.PalindromesUtils
-- Copyright   :  (c) 2007 - 2013 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- Flags a user can specify
-----------------------------------------------------------------------------
-- Palindromic variants (choose 1 out of 6; mutually exclusive):
-- Algorithm complexity (choose 1 out of 2; mutually exclusive):
-- Output format (choose 1 out of 4; mutually exclusive):
-- Modifiers (choose 0 to 5; where the length restrictions need to fit together)
-- input via AtLeast and AtMost. Adapt?
-- Input format
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Algorithms.Palindromes.PalindromesUtils
    ( Flag (..)
    , showPalindromeDNA
    , (=:=)
    , showPalindrome
    , showTextPalindrome
    , myIsLetterC
    , surroundedByPunctuation
    , listArrayl0
    , vecToArray
    , toDNA
    , Couplable
    , DNA (..)
    , couplableWithItself
    , couplableWithItselfAtIndex
    ) where

import Data.Array (Array, bounds, listArray, (!))
import Data.Char (isControl, isPunctuation, isSpace)
import Data.Foldable (Foldable (toList))

import qualified Data.Vector as V

data Flag
    = Help
    | Plain
    | Text
    | Word
    | DNA
    | Extend Int
    | Linear
    | Quadratic
    | Longest
    | LengthLongest
    | Maximal
    | LengthMaximal
    | Gap Int
    | NrOfErrors Int
    | LengthAtLeast Int
    | LengthAtMost Int
    | LengthExact Int
    | LengthBetween Int Int
    | StandardInput

-- | Data type to represent a single found palindrome
data Palindrome
    = Palindrome
    { palCenterIndex :: Int
    {- ^ The index of the center of this found palindrome from the pre-processed input
    vector.
    -}
    , palLength :: Int
    -- ^ The length of the found palindrome in the pre-processed input vector.
    , palStart :: Int
    {- ^ The (inclusive) starting character index of the found palindrome in the original
    (not pre-processed) input string
    -}
    , palEnd :: Int
    {- ^ The (exclusive) ending character index of the found palindrome in the original
    (not pre-processed) input string
    -}
    , palText :: String
    {- ^ The text representing the found palindrome. Note that this must be a string,
    not some abstract datatype. This string must be a subarray of the original
    (not pre-processed) input string, meaning that e.g. present punctuation is in this
    string.
    -}
    }

{- An example text Palindrome from plain input string "bab..ac" is
(Palindrome 5 3 1 6 "ab..a"). The center is on the 'b' and has center index 5. The
pre-processed text palindrome is "aba", so the length is 3, and after adding back
punctuation, the start character index is 1 (the first 'a') and the end character index
is 6 (the 'c' after the second 'a'). The string representing this text palindrome is
"ab..a". -}

{-
-------------------------------------
  Begin functions to show single palindromes
-------------------------------------
-}

{- |
  Show palindrome generates a string of a single palindrome that is contained in the input.
  The DNA version works with the assumption that every centre is between letters.
-}
showPalindromeDNA :: (Show a) => V.Vector a -> (Int, Int) -> String
showPalindromeDNA input (len, pos) =
    let startpos = pos - len `div` 2
    in  (show startpos ++)
            . (" to " ++)
            . (show (startpos + len) ++)
            . ("\t" ++)
            . (show (V.take len $ V.drop startpos input) ++)
            . ("\t" ++)
            $ show len

{- |
  Show palindrome generates a string of a single palindrome that is contained in the input.
  This version works with the assumption that centres can be on top and between letters.
-}
showPalindrome :: (Show a) => V.Vector a -> (Int, Int) -> String
showPalindrome input (len, pos) = show a
  where
    startpos = pos `div` 2 - len `div` 2
    a = V.take len $ V.drop startpos input

{- |
  Show palindrome generates a string of a single palindrome that is contained in the input.
  This version works with the assumption that centres can be on top and between letters.
  Due to the way `show` works with text symbols, this procedure is a bit longer than the generic version.
-}
showTextPalindrome
    :: V.Vector Char -> Array Int Int -> (Int, Int) -> String
showTextPalindrome input positionTextInput (len, pos) =
    let startpos = pos `div` 2 - len `div` 2
        endpos =
            if odd len
                then pos `div` 2 + len `div` 2
                else pos `div` 2 + len `div` 2 - 1
        (pfirst, plast) = bounds positionTextInput
        (ifirst, ilast) = (0, 1 + V.length input)
    in  if endpos < startpos
            then []
            else
                {-
                   This block of code is used to add punctuation at the start
                   and end of the palindrome to the returned value
                -}
                let start =
                        if startpos > pfirst
                            then (positionTextInput ! (startpos - 1)) + 1
                            else ifirst
                    end =
                        if endpos < plast
                            then (positionTextInput ! (endpos + 1)) - 1
                            else ilast
                in  toList $
                        V.filter
                            (\c -> c /= '\n' && c /= '\r')
                            (V.take (end - start + 1) (V.drop start input))

{-
-------------------------------------
  End functions to show single palindromes
-------------------------------------
-}

{-
--------------------------------------
  Begin punctuation utility functions
--------------------------------------
-}

{- |
  Checks whether a character is a letter
-}
myIsLetterC :: Char -> Bool
myIsLetterC c =
    not (isPunctuation c)
        && not (isControl c)
        && not (isSpace c)

{- |
  Checks whether the range specified by the first 2 parameters is surrounded by punctuation in the 3rd parameter.
    If begin is out of bounds, it still returns True if there is punctuation after index end.
    If end is out of bounds, it still returns True if there is punctuation before index begin.
    If both are out of bounds return True
    Note that out of bounds is considered True as this would be an end/start of a file which is considered punctuation
-}
surroundedByPunctuation :: Int -> Int -> V.Vector Char -> Bool
surroundedByPunctuation begin end input
    | begin > afirst && end < alast =
        not (myIsLetterC ((V.!) input (begin - 1)))
            && not (myIsLetterC ((V.!) input (end + 1))) -- returns if the positions around begin and end are surrounded
    | begin <= afirst && end < alast = not (myIsLetterC ((V.!) input (end + 1)))
    | begin <= afirst && end >= alast = True
    | begin > afirst && end >= alast = not (myIsLetterC ((V.!) input (begin - 1)))
    | otherwise = error "surroundedByPunctuation"
  where
    (afirst, alast) = (0, V.length input - 1)

{-
--------------------------------------
  End punctuation utility functions
--------------------------------------
-}

{-
----------------------------------------------------
  Begin foldable (Seq/Array/List) utility functions
----------------------------------------------------
-}

listArrayl0 :: [a] -> Array Int a
listArrayl0 string = listArray (0, length string - 1) string

vecToArray :: V.Vector a -> Array Int a
vecToArray v = listArray bound content
  where
    bound = (0, V.length v - 1)
    content = V.toList v

{-
-----------------------------
  Begin couplable definition
-----------------------------
-}

{- |
  Shows that some element belongs to another element.
  For example, A belongs to T in DNA, and 'z' belongs to 'z' in normal text.
-}
class Couplable a where
    (=:=) :: a -> a -> Bool

-- | Define Couplable instance for any a of class Eq. Just use the equality relation.
instance (Eq a) => Couplable a where
    (=:=) = (==)

{-
-----------------------------
  End couplable definition
-----------------------------
-}

{-
--------------------------------------
  Begin DNA definition and functions
--------------------------------------
-}

-- | Datatype for the different DNA, note that (=)/Eq is not suitable for checking if DNA has palindromes, instead couplable should be used
data DNA = A | T | C | G | N deriving (Show, Eq)

-- | Declare instance Couplable for DNA. A and T form a couple, C and G form a couple.
instance {-# OVERLAPPING #-} Couplable DNA where
    A =:= T = True
    T =:= A = True
    G =:= C = True
    C =:= G = True
    _ =:= _ = False

toDNA :: (Functor f) => f Char -> f DNA
toDNA = fmap charToDNA

charToDNA :: Char -> DNA
charToDNA 'A' = A
charToDNA 'T' = T
charToDNA 'C' = C
charToDNA 'G' = G
charToDNA 'N' = N
charToDNA 'a' = A
charToDNA 't' = T
charToDNA 'c' = C
charToDNA 'g' = G
charToDNA 'n' = N
charToDNA _ = error "Not a valid DNA string"

{-
--------------------------------------
  End DNA definition and functions
--------------------------------------
-}

{-
---------------------------------------
  Begin functions for Couplable types
---------------------------------------
-}

-- | Returns whether an element is couplable with itself.
couplableWithItself :: (Couplable a) => a -> Bool
couplableWithItself element = element =:= element

{- | Safe function which returns whether an element at an index in the input vector is
  couplable with itself.
-}
couplableWithItselfAtIndex :: (Couplable a) => V.Vector a -> Int -> Bool
couplableWithItselfAtIndex input index
    | index < 0 || index >= V.length input = False
    | otherwise = couplableWithItself element
  where
    element = input V.! index

{-
---------------------------------------
  End functions for Couplable types
---------------------------------------
-}
