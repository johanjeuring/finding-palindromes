module PalindromeProperties where
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Test.QuickCheck (Gen,Property,arbitrary,forAll)
import Data.Char (readLitChar,toLower)
import qualified Data.Algorithms.Palindromes.Palindromes as P
import qualified Data.Algorithms.Palindromes.PalindromesUtils as PU
import PalindromeMethods (longestTextPalindrome)

propPalindromesAroundCentres :: Property
propPalindromesAroundCentres = forAll (arbitrary:: Gen [Char]) $ \l -> 
    let 
        input = BC.pack l
        input' = B.map  PU.myToLower $ B.filter PU.myIsLetterW input
        posArray = PU.listArrayl0 $ B.findIndices PU.myIsLetterW input
    in 
        P.palindromesAroundCentres 
            (Just PU.Text) 
            (Just PU.Linear) 
            Nothing 
            Nothing 
            input 
            input'
            posArray -- Position array
        == longestPalindromesQ input'
            


longestPalindromesQ :: B.ByteString -> [Int]
longestPalindromesQ input =   
    let 
        (afirst,alast) = (0,B.length input - 1)
        positions = [0 .. 2*(alast-afirst+1)]
    in map (lengthPalindromeAround input) positions

lengthPalindromeAround :: B.ByteString -> Int -> Int
lengthPalindromeAround input position 
    | even position = 
        extendPalindromeAround (afirst+pos-1) (afirst+pos) 
    | odd position = 
        extendPalindromeAround (afirst+pos-1) (afirst+pos+1) 
  where  
    pos             =  div position 2
    (afirst,alast)  =  (0,B.length input -1)
    extendPalindromeAround start end  = 
        if start < 0 
            || end > alast-afirst 
            || B.index input start /= B.index input end
        then end-start-1
        else extendPalindromeAround (start-1) (end+1) 

propTextPalindrome :: Property
propTextPalindrome =
    forAll (arbitrary:: Gen [Char]) $ 
      \l -> 
          let ltp   =   longestTextPalindrome  PU.Linear (BC.pack l)
              ltp'  =  map toLower (filter  PU.myIsLetterC (unescape ltp))
          in ltp' == reverse ltp'

unescape :: String -> String
unescape [] = []
unescape cs = case readLitChar cs of
    (c,rest):xs -> c:unescape rest
    []          -> []  