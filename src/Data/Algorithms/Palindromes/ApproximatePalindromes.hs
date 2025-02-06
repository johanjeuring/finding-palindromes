-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Algorithms.Palindromes.ApproximatePalindromes
-- Copyright   :  (c) 2012 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.Algorithms.Palindromes.ApproximatePalindromes 
       (approximatePalindromes
       ,approximatePalindromesLengthAtLeast
       ,gappedApproximatePalindromesAroundCentres
       ,approximateTextPalindromes
       ,approximateTextPalindromesLengthAtLeast
       ,gappedApproximateTextPalindromesLengthAtLeast
       ) where

import Data.List (intercalate)
import Data.Array (Array()) 
import qualified Data.ByteString as B
import Data.Algorithms.Palindromes.PalindromesUtils (showPalindrome,showTextPalindrome,myIsLetterW,myToLower,listArrayl0,Flag(..))

approximatePalindromes         :: Flag -> Int -> B.ByteString -> String
approximatePalindromes outputFormat k input = case outputFormat of
  Maximal -> intercalate "\n" $ map (showPalindrome input) $ zip (approximatePalindromesAroundCentres input k) [0..]

approximatePalindromesLengthAtLeast :: Int -> Int -> B.ByteString -> String
approximatePalindromesLengthAtLeast k m input = 
    intercalate "\n" 
  $ map (showPalindrome input) 
  $ filter ((m<=) . fst)
  $ zip (approximatePalindromesAroundCentres input k) [0..]

approximatePalindromesAroundCentres :: B.ByteString -> Int -> [Int]
approximatePalindromesAroundCentres input k = 
  let centers       =  [0 .. 2*B.length input]
  in  map (lengthApproximatePalindromeAround input k) centers

lengthApproximatePalindromeAround :: B.ByteString -> Int -> Int -> Int
lengthApproximatePalindromeAround input k center 
  | even center = lengthApproximatePalindrome input lastPos k (c-1) c 
  | odd  center = lengthApproximatePalindrome input lastPos k (c-1) (c+1) 
  where  c        =  div center 2        
         lastPos  =  B.length input - 1
         
lengthApproximatePalindrome :: B.ByteString -> Int -> Int -> Int -> Int -> Int 
lengthApproximatePalindrome input lastPos k start end  
  |  start < 0 || end > lastPos                =  end-start-1
  |  B.index input start == B.index input end  =  lengthApproximatePalindrome input lastPos k (start-1) (end+1) 
  |  k > 0                                     =  lengthApproximatePalindrome input lastPos (k-1) (start-1) (end+1) 
  |  otherwise                                 =  end-start-1

gappedApproximatePalindromesAroundCentres :: B.ByteString -> Int -> Int -> [Int]
gappedApproximatePalindromesAroundCentres input g k = 
  let centers       =  [0 .. 2*B.length input]
  in  map (lengthGappedApproximatePalindromeAround input g k) centers

lengthGappedApproximatePalindromeAround :: B.ByteString -> Int -> Int -> Int -> Int
lengthGappedApproximatePalindromeAround input g k center 
  | even center = let halfg = div g 2 in lengthApproximatePalindrome input lastPos k (c-1-halfg) (c+halfg) 
  | odd  center = let halfg = if even g then div (g-1) 2 else div g 2 in lengthApproximatePalindrome input lastPos k (c-1-halfg) (c+1+halfg) 
  where  c        =  div center 2        
         lastPos  =  B.length input - 1

-----------------------------------------------------------------------------
-- approximateTextPalindromes
-----------------------------------------------------------------------------

approximateTextPalindromes :: Flag -> Int -> B.ByteString -> String
approximateTextPalindromes outputFormat k input = 
  let textInput          =  B.map myToLower (B.filter myIsLetterW input)
      positionTextInput  =  listArrayl0 (B.findIndices myIsLetterW input)
  in  case outputFormat of 
        Maximal -> approximateTextPalindromes' input k textInput positionTextInput 

-- BS is a bad name
approximateTextPalindromes' :: B.ByteString -> Int -> B.ByteString -> Array Int Int -> String
approximateTextPalindromes' input k textInput positionTextInput  = 
    intercalate "\n" 
  $ map (showTextPalindrome input positionTextInput) 
  $ (zip (approximatePalindromesAroundCentres textInput k) [0..])  
                      
approximateTextPalindromesLengthAtLeast :: Int -> Int -> B.ByteString -> String
approximateTextPalindromesLengthAtLeast k l input = 
  let textInput          =  B.map myToLower (B.filter myIsLetterW input)
      positionTextInput  =  listArrayl0 (B.findIndices myIsLetterW input)
  in  approximateTextPalindromesLengthAtLeast' input k l textInput positionTextInput 

approximateTextPalindromesLengthAtLeast' :: B.ByteString -> Int -> Int -> B.ByteString -> Array Int Int -> String
approximateTextPalindromesLengthAtLeast' input k l textInput positionTextInput  = 
    intercalate "\n" 
  $ map (showTextPalindrome input positionTextInput)
  $ filter ((l<) . fst) 
  $ (zip (approximatePalindromesAroundCentres textInput k) [0..])  

gappedApproximateTextPalindromesLengthAtLeast :: Int -> Int -> Int -> B.ByteString -> String
gappedApproximateTextPalindromesLengthAtLeast g k l input = 
  let textInput          =  B.map myToLower (B.filter myIsLetterW input)
      positionTextInput  =  listArrayl0 (B.findIndices myIsLetterW input)
  in  gappedApproximateTextPalindromesLengthAtLeast' input g k l textInput positionTextInput 

gappedApproximateTextPalindromesLengthAtLeast' :: B.ByteString -> Int -> Int -> Int -> B.ByteString -> Array Int Int -> String
gappedApproximateTextPalindromesLengthAtLeast' input g k l textInput positionTextInput  = 
    intercalate "\n" 
  $ map (showTextPalindrome input positionTextInput)
  $ filter ((l<) . fst) 
  $ (zip (gappedApproximatePalindromesAroundCentres textInput g k) [0..])  

