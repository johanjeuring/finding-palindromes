module Data.Algorithms.Palindromes.Palindrome where

-- | Data type to represent a single found palindrome
data Palindrome
    = Palindrome
    { palCenterIndex :: Int
    {- ^ The index of the center of this found palindrome from the pre-processed input
    vector.
    -}
    , palLength :: Int
    -- ^ The length of the found palindrome in the pre-processed input vector.
    , palText :: String
    {- ^ The text representing the found palindrome. Note that this must be a string,
    not some abstract datatype. This string must be a subarray of the original
    (not pre-processed) input string, meaning that e.g. present punctuation is in this
    string.
    -}
    , palRange :: (Int, Int)
    -- ^ The start (inclusive) and end (exclusive) index of the palindrome in the original string
    }
    deriving (Show, Eq)

{- An example text Palindrome from plain input string "bab..ac" is
(Palindrome 5 3 1 6 "ab..a"). The center is on the 'b' and has center index 5. The
pre-processed text palindrome is "aba", so the length is 3, and after adding back
punctuation, the start character index is 1 (the first 'a') and the end character index
is 6 (the 'c' after the second 'a'). The string representing this text palindrome is
"ab..a". -}