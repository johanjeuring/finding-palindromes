-- test strict integers
-- >let input = Data.ByteString.pack (map Data.ByteString.Internal.c2w "yabadabadoo")

-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Algorithms.Palindromes.Palindromes
-- Copyright   :  (c) 2007 - 2013 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------


module Data.Algorithms.Palindromes.Palindromes 
       (palindrome
       ,palindromesAroundCentres
       ,dnaLengthGappedApproximatePalindromeAround
       )  where
 
import Data.List (maximumBy,intercalate)
import qualified Data.ByteString as B
import Data.Algorithms.Palindromes.PalindromesUtils 
       (showPalindrome
       ,showPalindromeDNA
       ,showTextPalindrome
       ,myToLower
       ,myIsLetterW
       ,listArrayl0
       ,appendseq
       ,Flag(..)
       ,(=:=)
       ,surroundedByPunctuation
       )
import qualified Data.Sequence as S 
import Data.Word (Word8)
import Data.Array(Array,(!))


-----------------------------------------------------------------------------
-- palindrome dispatches to the desired variant of the palindrome finding
-- algorithm. It captures all the variablity, in input format, output format,
-- and length restrictions. Variability has been `pushed down' into the code
-- as much as possible, using extra arguments whenever needed, for example
-- for word palindromes (which have not been implemented correctly at the 
-- moment: I do get the longest word palindromes, but shorter ones may 
-- actually not be word palindromes).
-----------------------------------------------------------------------------

-- | palindrome captures all possible variants of finding palindromes.
palindrome        :: Maybe Flag -> Maybe Flag -> Maybe Flag -> Maybe Flag -> Maybe Flag -> Maybe Flag -> B.ByteString -> String
palindrome palindromeVariant outputFormat algorithmComplexity lengthModifier gap nrOfErrors input = 
  let predicate          =  case lengthModifier of
                              Just (LengthAtLeast m)    ->  (m<=)
                              Just (LengthAtMost  m)    ->  (<=m)
                              Just (LengthExact   m)    ->  \l -> m<=l && (odd l == odd m)
                              Just (LengthBetween m n)  ->  \pl -> pl >= m && pl <= n
                              _                         ->  const True
      post               =  case lengthModifier of
                              Just (LengthExact   m)    ->  \_ -> m  
                              _                         ->  id
      textinput          =  B.map myToLower (B.filter myIsLetterW input) 
      positionTextInput  =  listArrayl0 (B.findIndices myIsLetterW input)
      input'             =  case palindromeVariant of
                              Just Text                 ->  textinput
                              Just Word                 ->  textinput
                              _                         ->  input
      show'              =  case palindromeVariant of
                              Just Text                 ->  showTextPalindrome input positionTextInput
                              Just Word                 ->  showTextPalindrome input positionTextInput
                              Just DNA                  ->  showPalindromeDNA input
                              _                         ->  showPalindrome input
      outputf           =  case outputFormat of
                             Just LengthLongest         ->  show . maximum . map post . filter predicate 
                             Just Maximal               ->  intercalate "\n" . map show' . map (\(l,r) -> (post l,r)) . filter (predicate . fst) . flip zip [0..] 
                             Just LengthMaximal         ->  show . map post . filter predicate 
                             _                          ->  show' . maximumBy (\(l,_) (l',_) -> compare l l') . map (\(l,r) -> (post l,r)) . filter (predicate . fst) . flip zip [0..] 
  in outputf $ palindromesAroundCentres palindromeVariant algorithmComplexity gap nrOfErrors input input' positionTextInput

{- 
-- The following code is replaced by the equivalent code using a more efficient
-- data structure. It is kept here because this is most probably easier to understand,
-- and it is the code explained on the blog.

-----------------------------------------------------------------------------
-- palindromesAroundCentres 
--
-- The function that implements the palindrome finding algorithm.
-- Used in all the above interface functions.
-----------------------------------------------------------------------------

-- | palindromesAroundCentres is the central function of the module. It returns
--   the list of lenghths of the longest palindrome around each position in a
--   string.
palindromesAroundCentres        :: B.ByteString -> [Int]
palindromesAroundCentres input  =  reverse $ extendPalindrome input 0 0 []

extendPalindrome :: B.ByteString -> Int -> Int -> [Int] -> [Int]
extendPalindrome input rightmost currentPalindrome currentMaximalPalindromes 
  | rightmost > last
      -- reached the end of the array
      =  finalPalindromes currentPalindrome currentMaximalPalindromes (currentPalindrome:currentMaximalPalindromes)
  | rightmost-currentPalindrome == first ||
    not (B.index input rightmost == B.index input (rightmost-currentPalindrome-1))
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
      -- the last centre is on the last element: try to extend the tail of length 1
      =  extendPalindrome input (rightmost+1) 1 currentMaximalPalindromes
  | nrOfCenters-1 == head previousMaximalPalindromes
      -- the previous element in the centre list reaches exactly to the end of the last 
      -- tail palindrome use the mirror property of palindromes to find the longest tail palindrome
      =  extendPalindrome input rightmost (head previousMaximalPalindromes) currentMaximalPalindromes
  | otherwise
      -- move the centres one step add the length of the longest palindrome to the centres
      =  moveCenter input rightmost (min (head previousMaximalPalindromes) (nrOfCenters-1):currentMaximalPalindromes) (tail previousMaximalPalindromes) (nrOfCenters-1)

finalPalindromes :: Int -> [Int] -> [Int] -> [Int]
finalPalindromes nrOfCenters previousMaximalPalindromes currentMaximalPalindromes  
  | nrOfCenters == 0
      =  currentMaximalPalindromes
  | nrOfCenters > 0
      =  finalPalindromes (nrOfCenters-1) (tail previousMaximalPalindromes) (min (head previousMaximalPalindromes) (nrOfCenters-1):currentMaximalPalindromes)
  | otherwise  
      =  error "finalCentres: input < 0"               

-}

-----------------------------------------------------------------------------
-- palindromesAroundCentresS 
--
-- The function that implements the palindrome finding algorithm.
-- Used in all the above interface functions.
-- 
-- I use the Seq datatype to pass on the maximal palindromes that are used for 
-- finding the maximal palindromes to the right of the center of the current
-- longest tail paindrome.
-----------------------------------------------------------------------------

-- | palindromesAroundCentres is the central function of the module. It returns
--   the list of lenghths of the longest palindrome around each position in a
--   string. 
palindromesAroundCentres        :: Maybe Flag -> Maybe Flag -> Maybe Flag -> Maybe Flag -> B.ByteString -> B.ByteString -> Array Int Int -> [Int]
palindromesAroundCentres palindromeVariant algorithmComplexity gap nrOfErrors input input' positionTextInput = 
  case (algorithmComplexity,gap,nrOfErrors) of
    (Just Linear   ,Nothing,Nothing)  ->  case palindromeVariant of
                                            Just DNA   ->  reverse $ appendseq $ extendPalindromeS 2 0 input' [] S.empty 0 0 
                                            Just Word  ->  reverse $ map (head . snd) $ extendTailWord input input' positionTextInput [] 0 (0,[0]) 
                                            _          ->  reverse $ appendseq $ extendPalindromeS 1 1 input' [] S.empty 0 0 
    (Just Linear   ,_      ,_      )  ->  error "palindromesAroundCentres: cannot calculate approximate or gapped palindromes using the linear-time algorithm"  
    (Just Quadratic,g      ,k      )  ->  let g'  =  case g of
                                                       Just (Gap g'')         ->  g''
                                                       _                      ->  0
                                              k'  =  case k of
                                                       Just (NrOfErrors k'')  ->  k''
                                                       _                      ->  0   
                                          in  gappedApproximatePalindromesAroundCentres palindromeVariant input g' k' 
    (_             ,_      ,_      )  ->  error "palindromesAroundCentres: case not defined"

extendPalindromeS :: Int -> Int -> B.ByteString -> [Int] -> S.Seq Int -> Int -> Int -> ([Int],S.Seq Int)
extendPalindromeS centerfactor tailfactor input = 
  let ePS maximalPalindromesPre maximalPalindromesIn rightmost currentPalindrome 
        | rightmost > lastPos
          -- reached the end of the array
          =  finalPalindromesS centerfactor currentPalindrome maximalPalindromesPre (currentPalindrome S.<| maximalPalindromesIn) maximalPalindromesIn
        | rightmost-currentPalindrome == first ||
          not (B.index input rightmost == B.index input (rightmost-currentPalindrome-1))
            -- the current palindrome extends to the start of the array, 
            -- or it cannot be extended 
            =  mCS rightmost maximalPalindromesPre (currentPalindrome S.<| maximalPalindromesIn) maximalPalindromesIn currentPalindrome 
        | otherwise                                           
            -- the current palindrome can be extended
            =  let (left,rest) = splitAt 2 maximalPalindromesPre
               in  ePS rest (foldr (flip (S.|>)) maximalPalindromesIn left) (rightmost+1) (currentPalindrome+2) 
        where  first = 0
               lastPos  = B.length input - 1
      mCS rightmost maximalPalindromesPre maximalPalindromesIn maximalPalindromesIn' nrOfCenters
        | nrOfCenters == 0
          -- the last centre is on the last element: try to extend the tail of length 1
          =  ePS maximalPalindromesPre maximalPalindromesIn (rightmost+1) tailfactor 
        | nrOfCenters-centerfactor == S.index maximalPalindromesIn' 0
          -- the previous element in the centre list reaches exactly to the end of the last 
          -- tail palindrome use the mirror property of palindromes to find the longest tail palindrome
          =  ePS maximalPalindromesPre maximalPalindromesIn rightmost (nrOfCenters-centerfactor)
        | otherwise
          -- move the centres one step add the length of the longest palindrome to the centres
          =  case S.viewl maximalPalindromesIn' of
               headq S.:< tailq -> mCS rightmost maximalPalindromesPre (min headq (nrOfCenters-centerfactor) S.<| maximalPalindromesIn) tailq (nrOfCenters-centerfactor)
               S.EmptyL         -> error "extendPalindromeS: empty sequence"
  in ePS

-- moveCenterS :: B.ByteString -> Int -> [Int] -> S.Seq Int -> S.Seq Int -> Int -> ([Int],S.Seq Int)

finalPalindromesS :: Int -> Int -> [Int] -> S.Seq Int -> S.Seq Int -> ([Int],S.Seq Int)
finalPalindromesS centerfactor nrOfCenters maximalPalindromesPre maximalPalindromesIn maximalPalindromesIn'  
  | nrOfCenters == 0
      =  (maximalPalindromesPre,maximalPalindromesIn)
  | nrOfCenters > 0
      =  case S.viewl maximalPalindromesIn' of
	       headq S.:< tailq -> finalPalindromesS centerfactor (nrOfCenters-centerfactor) maximalPalindromesPre (min headq (nrOfCenters-centerfactor) S.<| maximalPalindromesIn) tailq 
	       S.EmptyL         -> error "finalPalindromesS: empty sequence"
  | otherwise  
      =  error "finalPalindromesS: input < 0"              

gappedApproximatePalindromesAroundCentres :: Maybe Flag -> B.ByteString -> Int -> Int -> [Int]
gappedApproximatePalindromesAroundCentres palindromeVariant input g k = 
  case palindromeVariant of
    Just DNA -> map (lengthGappedApproximatePalindromeAround (=:=) 1 input g k) (if even g then [0 .. B.length input] else [0 .. B.length input-1])
    _        -> map (lengthGappedApproximatePalindromeAround (==)      2 input g k) [0 .. 2*B.length input]

-- I probably get the wrong positions printed for odd-gapped palindromes
-- the next two functions should be mergable, with a centerdivfactor
lengthGappedApproximatePalindromeAround :: (Word8 -> Word8 -> Bool) -> Int -> B.ByteString -> Int -> Int -> Int -> Int
lengthGappedApproximatePalindromeAround (===) centerfactor input g k center =
  let halfg        =  div g 2
      c            =  div center centerfactor
      lengthInput  =  B.length input
      halfg'       |  c < halfg                =  c
                   |  c + halfg > lengthInput  =  lengthInput-c
                   |  otherwise                =  halfg
      left         =  c-1-halfg'
      right        =  if even g then c+halfg' else c+1+halfg' 
  in  lengthApproximatePalindrome (===) input k left right

lengthApproximatePalindrome :: (Word8 -> Word8 -> Bool) -> B.ByteString  -> Int -> Int -> Int -> Int 
lengthApproximatePalindrome (===) input k start end  
  |  start < 0 || end > lastPos                 =  end-start-1
  |  B.index input start === B.index input end  =  lengthApproximatePalindrome (===) input k (start-1) (end+1) 
  |  k > 0                                      =  lengthApproximatePalindrome (===) input (k-1) (start-1) (end+1) 
  |  otherwise                                  =  end-start-1
  where lastPos = B.length input - 1
         
dnaLengthGappedApproximatePalindromeAround :: Maybe Flag -> Maybe Flag -> Int -> B.ByteString -> String
dnaLengthGappedApproximatePalindromeAround (Just (Gap gap)) (Just (NrOfErrors k)) center input = show $ lengthGappedApproximatePalindromeAround (=:=) 1 input gap k center 
dnaLengthGappedApproximatePalindromeAround _                _                     center input = show $ lengthGappedApproximatePalindromeAround (=:=) 1 input 0   0 center 

extendTailWord :: B.ByteString -> B.ByteString -> Array Int Int -> [(Int,[Int])] -> Int -> (Int,[Int]) -> [(Int,[Int])] 
extendTailWord input textInput positionTextInput centres n current@(currentTail,currentTailWords)  
  | n > alast                          =  
      -- reached the end of the text input array                                     
      finalWordCentres input textInput positionTextInput (current:centres) currentTail centres (1+length centres)
  | n-currentTail == afirst            =  
      -- the current longest tail palindrome extends to the start of the text input array
      extendWordCentres input textInput positionTextInput (current:centres) n centres currentTail
  | B.index textInput n == B.index textInput (n-currentTail-1)     =  
      -- the current longest tail palindrome can be extended
      -- check whether or not the extended palindrome is a wordpalindrome
      if surroundedByPunctuation (positionTextInput!(n-currentTail-1)) (positionTextInput!n) input
      then extendTailWord input textInput positionTextInput centres (n+1) (currentTail+2,currentTail+2:currentTailWords) 
      else extendTailWord input textInput positionTextInput centres (n+1) (currentTail+2,currentTailWords)       
  | otherwise                          =  
      -- the current longest tail palindrome cannot be extended                 
      extendWordCentres input textInput positionTextInput (current:centres) n centres currentTail
  where  (afirst,alast)  =  (0,B.length textInput -1)

extendWordCentres :: B.ByteString -> B.ByteString -> Array Int Int -> [(Int,[Int])] -> Int -> [(Int,[Int])] -> Int -> [(Int,[Int])]
extendWordCentres input textInput positionTextInput centres n tcentres centreDistance
  | centreDistance == 0                =  
      -- the last centre is on the last element: 
      -- try to extend the tail of length 1
      if surroundedByPunctuation (positionTextInput!n) (positionTextInput!n) input
      then extendTailWord input textInput positionTextInput centres (n+1) (1,[1,0]) 
      else extendTailWord input textInput positionTextInput centres (n+1) (1,[0]) 
  | centreDistance-1 == fst (head tcentres)  =  
      -- the previous element in the centre list 
      -- reaches exactly to the end of the last 
      -- tail palindrome use the mirror property 
      -- of palindromes to find the longest tail 
      -- palindrome
      let (currentTail,oldWord:oldWords) = head tcentres
      in if surroundedByPunctuation (positionTextInput!(n-currentTail)) (positionTextInput!(n-1)) input
         then if oldWord == currentTail
		      then extendTailWord input textInput positionTextInput centres n (head tcentres) 
		      else extendTailWord input textInput positionTextInput centres n (currentTail,currentTail:oldWord:oldWords) 
		 else if oldWord == currentTail && oldWord > 0
			  then extendTailWord input textInput positionTextInput centres n (currentTail, tail (snd (head tcentres)))  
		      else extendTailWord input textInput positionTextInput centres n (head tcentres) 
  | otherwise                          =  
      -- move the centres one step
      -- add the length of the longest palindrome 
      -- to the centres
      let newTail   =  min (fst (head tcentres)) (centreDistance-1)
          oldWord   =  head (snd (head tcentres))
          newWords | oldWord < newTail  
	                   = if surroundedByPunctuation (positionTextInput!(n-newTail+1)) (positionTextInput!n) input
		                 then newTail:snd (head tcentres) 
		                 else snd (head tcentres) 
		            | null (tail (snd (head tcentres)))
			           = snd (head tcentres) 
			        | otherwise 
			           = tail (snd (head tcentres))
      in extendWordCentres input textInput positionTextInput ((newTail,newWords):centres) n (tail tcentres) (centreDistance-1)

finalWordCentres :: B.ByteString -> B.ByteString -> Array Int Int -> [(Int,[Int])] -> Int -> [(Int,[Int])] -> Int -> [(Int,[Int])]
finalWordCentres input textInput positionTextInput centres n tcentres mirrorPoint 
  | n == 0     =  centres
  | n > 0      =  let tlast                       =  B.length textInput - 1
                      (oldTail,oldWord:oldWords)  =  head tcentres
                      newTail                     =  min oldTail (n-1)
                      newWord                     =  min oldWord (n-1)
                      tailFirstMirror             =  min tlast (div (mirrorPoint - newTail) 2)
                      tailLastMirror              =  min tlast (if odd newTail then div (mirrorPoint + newTail) 2 else div (mirrorPoint + newTail) 2 - 1)
                      wordFirstMirror             =  min tlast (div (mirrorPoint - newWord) 2)
                      wordLastMirror              =  min tlast (if odd newWord then div (mirrorPoint + newTail) 2 else div (mirrorPoint + newTail) 2 - 1)
                      newWords | surroundedByPunctuation (positionTextInput!tailFirstMirror) (positionTextInput!tailLastMirror) input
		                                             =    if newWord == newTail
	                                                      then newTail:oldWords
		                                                  else newTail:oldWord:oldWords
		                       | surroundedByPunctuation (positionTextInput!wordFirstMirror) (positionTextInput!wordLastMirror) input ||
			                     null oldWords       =    newWord:oldWords
			                   | otherwise           =    oldWords
                 in  finalWordCentres input textInput positionTextInput ((newTail,newWords):centres) (n-1) (tail tcentres)  (mirrorPoint+1)
  | otherwise  = error "finalWordCentres: input < 0"        

