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


module Data.Algorithms.Palindromes.PalindromesUtils 
       (Flag(..)
       ,negateDNA
       ,showPalindromeDNA
       ,(=:=)
       ,showPalindrome
       ,showTextPalindrome
       ,myIsLetterC
       ,myIsLetterW
       ,myToLower
       ,surroundedByPunctuation
       ,appendseq
       ,listArrayl0
       )  where
 
import Data.Word (Word8)
import Data.Char (toLower,toUpper,isPunctuation,isSpace,isControl)
import Data.Array (Array,bounds,listArray,(!)) 
import qualified Data.ByteString as B
import Data.ByteString.Internal (w2c,c2w)
import qualified Data.Sequence as S

-----------------------------------------------------------------------------
-- Flags a user can specify
-----------------------------------------------------------------------------

data Flag  =  -- Palindromic variants (choose 1 out of 6; mutually exclusive):
              Help
           |  Plain
           |  Text
           |  Word
           |  DNA
           |  Extend Int 
              -- Algorithm complexity (choose 1 out of 2; mutually exclusive):
           |  Linear
           |  Quadratic
              -- Output format (choose 1 out of 4; mutually exclusive):
           |  Longest 
           |  LengthLongest 
           |  Maximal  
           |  LengthMaximal
              -- Modifiers (choose 0 to 5; where the length restrictions need to fit together)
           |  Gap Int 
           |  NrOfErrors Int 
           |  LengthAtLeast Int 
           |  LengthAtMost Int 
           |  LengthExact Int
           |  LengthBetween Int Int -- input via AtLeast and AtMost. Adapt?
              -- Input format
           |  StandardInput 

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
l =:= r  =  let cl = toUpper (w2c l)
                cr = toUpper (w2c r)
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

-----------------------------------------------------------------------------
-- Showing palindromes and other text related functionality
-----------------------------------------------------------------------------

showPalindrome :: B.ByteString -> (Int,Int) -> String
showPalindrome input (len,pos) = 
  let startpos = pos `div` 2 - len `div` 2
  in show $ B.take len $ B.drop startpos input 

showTextPalindrome :: B.ByteString -> Array Int Int -> (Int,Int) -> String
showTextPalindrome input positionTextInput (len,pos) = 
  let startpos   =  pos `div` 2 - len `div` 2
      endpos     =  if odd len 
                    then pos `div` 2 + len `div` 2 
                    else pos `div` 2 + len `div` 2 - 1
      (pfirst,plast) = bounds positionTextInput
      (ifirst,ilast) = (0,1 + B.length input)
  in  if endpos < startpos
      then []
      else let start      =  if startpos > pfirst
                             then (positionTextInput!(startpos-1))+1
                             else ifirst 
               end        =  if endpos < plast
                             then (positionTextInput!(endpos+1))-1
                             else ilast
           in  show (B.take (end-start+1) (B.drop start input))

{- Using this code instead of the last else above shows text palindromes without 
   all punctuation around it. Right now this punctuation is shown.

      else let start      =  positionArray!!!startpos
               end        =  positionArray!!!endpos
-}

-- For palindromes in strings, punctuation, spacing, and control characters
-- are often ignored

myIsLetterW     ::  Word8 -> Bool
myIsLetterW c'  =   not (isPunctuation c)
                &&  not (isControl c)
                &&  not (isSpace c)
  where c = w2c c'

myIsLetterC    ::  Char -> Bool
myIsLetterC c  =   not (isPunctuation c)
               &&  not (isControl c)
               &&  not (isSpace c)

myToLower  :: Word8 -> Word8
myToLower  = c2w . toLower . w2c

surroundedByPunctuation :: Int -> Int -> B.ByteString -> Bool
surroundedByPunctuation begin end input 
  | begin > afirst  && end < alast   =  not (myIsLetterW (B.index input (begin-1))) && not (myIsLetterW (B.index input (end+1)))
  | begin <= afirst && end < alast   =  not (myIsLetterW (B.index input (end+1)))
  | begin <= afirst && end >= alast  =  True
  | begin > afirst  && end >= alast  =  not (myIsLetterW (B.index input (begin-1)))
  | otherwise                        =  error "surroundedByPunctuation"
  where (afirst,alast) = (0,B.length input - 1)

-----------------------------------------------------------------------------
-- Seq utils
-----------------------------------------------------------------------------

appendseq :: ([a],S.Seq a) -> [a]
appendseq (list,s) = tolist s ++ list

tolist :: S.Seq a -> [a]
tolist s = case S.viewl s of 
               S.EmptyL -> []
               a S.:< r -> a:tolist r

-----------------------------------------------------------------------------
-- Array utils
-----------------------------------------------------------------------------

listArrayl0         :: [a] -> Array Int a
listArrayl0 string  =  listArray (0,length string - 1) string
