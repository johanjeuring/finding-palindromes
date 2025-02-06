-- >let input = B.pack (map BI.c2w "AATAATT")
-- >dnaPalindromesAroundCentres input

-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Algorithms.Palindromes.DNAPalindromes
-- Copyright   :  (c) 2012 - 2013 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.Algorithms.Palindromes.DNAPalindromes
       (extendPalindromeS
       ,negateDNA
       ,showPalindromeDNA
       ,(=:=)
       )  where
 
import Data.Char (toUpper)
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.Sequence as S
import Data.Algorithms.Palindromes.PalindromesUtils (appendseq,Flag(..))

{-
-----------------------------------------------------------------------------
-- dnaPalindromesAroundCentres 
--
-- The function that implements the palindrome finding algorithm.
-- Used in all the above interface functions.
-----------------------------------------------------------------------------

-- | dnaPalindromesAroundCentres is the central function of the module. It 
--   returns the list of lenghths of maximal even-length palindromes around 
--   each position in a string.
dnaPalindromesAroundCentres        :: B.ByteString -> [Int]
dnaPalindromesAroundCentres input  =  reverse $ extendPalindrome input 0 0 []

extendPalindrome :: B.ByteString -> Int -> Int -> [Int] -> [Int]
extendPalindrome input rightmost currentPalindrome currentMaximalPalindromes 
  | rightmost > last                                                 
      -- reached the end of the array                                     
      =  finalPalindromes currentPalindrome currentMaximalPalindromes (currentPalindrome:currentMaximalPalindromes)
  | rightmost-currentPalindrome == first ||
    not (B.index input rightmost =:= B.index input (rightmost-currentPalindrome-1))                                    
      -- the current palindrome extends to the start of the array, 
      -- or it cannot be extended 
      =  moveCenter input rightmost (currentPalindrome:currentMaximalPalindromes) currentMaximalPalindromes currentPalindrome 
  | otherwise  
      -- the current palindrome can be extended
      =  extendPalindrome input (rightmost+1) (currentPalindrome+2) currentMaximalPalindromes      
  where  first = 0
         last  = B.length input - 1

moveCenter :: B.ByteString -> Int -> [Int] -> [Int] -> Int -> [Int]
moveCenter input rightmost currentMaximalPalindromes previousMaximalPalindromes nrOfCenters
  | nrOfCenters == 0                
      -- the last centre is on the last element: try to extend the tail of length 0
      =  extendPalindrome input (rightmost+1) 0 currentMaximalPalindromes
  | nrOfCenters-2 == head previousMaximalPalindromes  
      -- the previous element in the centre list reaches exactly to the end of the last 
      -- tail palindrome use the mirror property of palindromes to find the longest tail palindrome
      =  extendPalindrome input rightmost (head previousMaximalPalindromes) currentMaximalPalindromes
  | otherwise                          
      -- move the centres one step add the length of the longest palindrome to the centres
      =  moveCenter input rightmost (min (head previousMaximalPalindromes) (nrOfCenters-2):currentMaximalPalindromes) (tail previousMaximalPalindromes) (nrOfCenters-2)

finalPalindromes :: Int -> [Int] -> [Int] -> [Int]
finalPalindromes nrOfCenters previousMaximalPalindromes currentMaximalPalindromes  
  | nrOfCenters == 0     
      =  currentMaximalPalindromes
  | nrOfCenters > 0      
      =  finalPalindromes (nrOfCenters-2) (tail previousMaximalPalindromes) (min (head previousMaximalPalindromes) (nrOfCenters-2):currentMaximalPalindromes)
  | otherwise  
      =  error "finalCentres: input < 0"               
-}

-----------------------------------------------------------------------------
-- dnaPalindromesAroundCentres 
--
-- The function that implements the palindrome finding algorithm.
-- Used in all the above interface functions.
-----------------------------------------------------------------------------

extendPalindromeS :: B.ByteString -> [Int] -> S.Seq Int -> Int -> Int -> ([Int],S.Seq Int)
extendPalindromeS input = 
  let ePS maximalPalindromesPre maximalPalindromesIn rightmost currentPalindrome 
        | rightmost > lastPos
          -- reached the end of the array
          =  finalPalindromesS currentPalindrome maximalPalindromesPre (currentPalindrome S.<| maximalPalindromesIn) maximalPalindromesIn
        | rightmost-currentPalindrome == first ||
          not (B.index input rightmost =:= B.index input (rightmost-currentPalindrome-1))
            -- the current palindrome extends to the start of the array, 
            -- or it cannot be extended 
            =  mCS rightmost maximalPalindromesPre (currentPalindrome S.<| maximalPalindromesIn) maximalPalindromesIn currentPalindrome 
        | otherwise                                           
            -- the current palindrome can be extended
            =  let (left,rest) = splitAt 2 maximalPalindromesPre
               in  ePS rest (foldr (flip (S.|>)) maximalPalindromesIn left) (rightmost+1) (currentPalindrome+2) 
        where  first = 0
               lastPos  = B.length input - 1
      mCS :: Int -> [Int] -> S.Seq Int -> S.Seq Int -> Int -> ([Int],S.Seq Int)
      mCS rightmost maximalPalindromesPre maximalPalindromesIn maximalPalindromesIn' nrOfCenters
        | nrOfCenters == 0
          -- the last centre is on the last element: try to extend the empty tail
          =  ePS maximalPalindromesPre maximalPalindromesIn (rightmost+1) 0 
        | nrOfCenters-2 == S.index maximalPalindromesIn' 0
          -- the previous element in the centre list reaches exactly to the end of the last 
          -- tail palindrome use the mirror property of palindromes to find the longest tail palindrome
          =  ePS maximalPalindromesPre maximalPalindromesIn rightmost (nrOfCenters-2)
        | otherwise
          -- move the centres one step add the length of the longest palindrome to the centres
          =  case S.viewl maximalPalindromesIn' of
               headq S.:< tailq -> mCS rightmost maximalPalindromesPre (min headq (nrOfCenters-2) S.<| maximalPalindromesIn) tailq (nrOfCenters-2)
               S.EmptyL         -> error "extendPalindromeS"
  in ePS

finalPalindromesS :: Int -> [Int] -> S.Seq Int -> S.Seq Int -> ([Int],S.Seq Int)
finalPalindromesS nrOfCenters maximalPalindromesPre maximalPalindromesIn maximalPalindromesIn'  
  | nrOfCenters == 0
      =  (maximalPalindromesPre,maximalPalindromesIn)
  | nrOfCenters > 0
      =  case S.viewl maximalPalindromesIn' of
           headq S.:< tailq -> finalPalindromesS (nrOfCenters-2) maximalPalindromesPre (min headq (nrOfCenters-2) S.<| maximalPalindromesIn) tailq 
           S.EmptyL         -> error "finalPalindromesS"
  | otherwise  
      =  error "finalCentres: input < 0"               

-----------------------------------------------------------------------------
-- Gapped approximate palindromes in DNA
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-- Equality on DNA
-----------------------------------------------------------------------------

negateDNA      :: Char -> Char
negateDNA 'A'  =  'T'
negateDNA 'T'  =  'A'
negateDNA 'C'  =  'G'
negateDNA 'G'  =  'C'
negateDNA _    =  error "negateDNA: not a DNA character"

(=:=)    :: Word8 -> Word8 -> Bool
l =:= r  =  let cl = toUpper (BI.w2c l)
                cr = toUpper (BI.w2c r)
            in if cl `elem` "ATCG" && cr `elem` "ATCG" 
	           then cl == negateDNA cr
	           else False

-----------------------------------------------------------------------------
-- Showing DNA palindromes
-----------------------------------------------------------------------------

showPalindromeDNA :: B.ByteString -> (Int,Int) -> String
showPalindromeDNA input (len,pos) = 
  let startpos = pos - len `div` 2
  in   (show startpos ++) 
     . (" to " ++) 
     . (show (startpos+len) ++) 
     . ("\t" ++) 
     . (show (B.take len $ B.drop startpos input) ++) 
     . ("\t" ++) 
     $ show len
