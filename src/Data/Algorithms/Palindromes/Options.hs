-- Did not yet translate all options. Complete the table in dispatchFlags.
-- Default doesn't work yet

-----------------------------------------------------------------------------
--
-- Module      :  Data.Algorithms.Palindromes.Options
-- Copyright   :  (c) 2007 - 2013 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Data.Algorithms.Palindromes.Options where

import System.Console.GetOpt 

import qualified Data.ByteString as B

import Data.Algorithms.Palindromes.PalindromesUtils (Flag(..))
import Data.Algorithms.Palindromes.Palindromes (palindrome,dnaLengthGappedApproximatePalindromeAround)

-----------------------------------------------------------------------------
-- Options
-----------------------------------------------------------------------------

-- I am using single letter options here (except for help): getOpt handles 
-- options too flexible: in case a letter within a multiple letter option is 
-- recognized, it is taken as a single letter option.
options :: [OptDescr Flag] 
options = 
  [Option "h" ["help"] (NoArg Help)
     "This message"
  ,Option "p" [] (NoArg Plain) 
     "Plain palindrome (default)"
  ,Option "t" [] (NoArg Text)
     "Palindrome ignoring case, spacing and punctuation"
  ,Option "w" [] (NoArg Word)
     "Palindrome surrounded by punctuation (if any)"
  ,Option "d" [] (NoArg DNA)
     "DNA palindrome"
  ,Option "l" [] (NoArg Longest)
     "Longest palindrome (deafult)"
  ,Option "e" [] (NoArg LengthLongest)
     "Length of the longest palindrome"
  ,Option "m" [] (NoArg Maximal)
     "Maximal palindrome around each position in the input"
  ,Option "a" [] (NoArg LengthMaximal)
     "Length of the maximal palindrome around each position in the input"
  ,Option "g" [] (ReqArg (Gap . (read :: String -> Int)) "arg")
     "Allow a gap of length [arg] in the palindrome"
  ,Option "n" [] (ReqArg (NrOfErrors . (read :: String -> Int)) "arg")
     "Allow at most [arg] errors in the palindrome"
  ,Option "b" [] (ReqArg (LengthAtLeast . (read :: String -> Int)) "arg")
     "Maximal palindromes of length at least [arg]"
  ,Option "c" [] (ReqArg (LengthAtMost . (read :: String -> Int)) "arg")
     "Maximal palindromes (possibly cut off) of length at most [arg]"
  ,Option "f" [] (ReqArg (LengthExact . (read :: String -> Int)) "arg")
     "Maximal palindromes (possibly cut off) of length exactly [arg]"
  ,Option "r" [] (NoArg Linear)
     "Use the linear algorithm"
  ,Option "q" [] (NoArg Quadratic)
     "Use the potentially quadratic algorithm (default)"
  ,Option "i" [] (NoArg StandardInput)
     "Read input from standard input"
  ,Option "x" [] (ReqArg (Extend . (read :: String -> Int)) "arg")
     "Extend a palindrome around center [arg]"
  ]

isHelp :: Flag -> Bool
isHelp Help                                          =  True
isHelp _                                             =  False      

isPlain :: Flag -> Bool
isPlain Plain                                        =  True
isPlain _                                            =  False      

isText :: Flag -> Bool
isText Text                                          =  True
isText _                                             =  False      

isWord :: Flag -> Bool
isWord Word                                          =  True
isWord _                                             =  False      

isDNA :: Flag -> Bool
isDNA DNA                                            =  True
isDNA _                                              =  False      

isLongest :: Flag -> Bool
isLongest Longest                                    =  True
isLongest _                                          =  False      

isLengthLongest :: Flag -> Bool
isLengthLongest LengthLongest                        =  True
isLengthLongest _                                    =  False      

isMaximal :: Flag -> Bool
isMaximal Maximal                                    =  True
isMaximal _                                          =  False      

isLengthMaximal :: Flag -> Bool
isLengthMaximal LengthMaximal                        =  True
isLengthMaximal _                                    =  False      

isGap :: Flag -> Bool
isGap (Gap _)                                        =  True
isGap _                                              =  False      

isNrOfErrors :: Flag -> Bool
isNrOfErrors (NrOfErrors _)                          =  True
isNrOfErrors _                                       =  False      

isLengthAtLeast :: Flag -> Bool
isLengthAtLeast (LengthAtLeast _)                    =  True
isLengthAtLeast _                                    =  False      

isLengthAtMost :: Flag -> Bool
isLengthAtMost (LengthAtMost _)                      =  True
isLengthAtMost _                                     =  False      

isLengthExact :: Flag -> Bool
isLengthExact (LengthExact _)                        =  True
isLengthExact _                                      =  False      

isLinear :: Flag -> Bool
isLinear Linear                                      =  True
isLinear _                                           =  False      

isQuadratic :: Flag -> Bool
isQuadratic Quadratic                                =  True
isQuadratic _                                        =  False      

isStandardInput :: Flag -> Bool
isStandardInput StandardInput                        =  True
isStandardInput _                                    =  False      

isExtend :: Flag -> Bool
isExtend (Extend _)                                  =  True
isExtend _                                           =  False      

palindromeVariant      :: [Flag] -> Maybe Flag
palindromeVariant flags
  | any isHelp   flags  =  Just Help
  | any isPlain  flags  =  Just Plain
  | any isText   flags  =  Just Text
  | any isWord   flags  =  Just Word
  | any isDNA    flags  =  Just DNA
  | any isExtend flags  =  Just $ head $ filter isExtend flags
  | otherwise           =  Nothing

outputFormat                  :: [Flag] -> Maybe Flag
outputFormat flags
  | any isLongest       flags  =  Just Longest
  | any isLengthLongest flags  =  Just LengthLongest
  | any isMaximal       flags  =  Just Maximal
  | any isLengthMaximal flags  =  Just LengthMaximal
  | otherwise                  =  Nothing

algorithmComplexity                  :: [Flag] -> Maybe Flag
algorithmComplexity flags
  | any isLinear       flags  =  Just Linear
  | any isQuadratic    flags  =  Just Quadratic
  | otherwise                 =  Nothing

lengthModifier :: [Flag] -> Maybe Flag
lengthModifier flags
  | any isLengthExact      flags  =  Just $ head $ filter isLengthExact flags
  |    any isLengthAtLeast flags  
    && any isLengthAtMost  flags  =  Just $ LengthBetween (getLengthAtLeasts flags) (getLengthAtMosts flags)
  | any isLengthAtLeast    flags  =  Just $ head $ filter isLengthAtLeast flags
  | any isLengthAtMost     flags  =  Just $ head $ filter isLengthAtMost flags
  | otherwise                     =  Nothing

gap :: [Flag] -> Maybe Flag
gap flags 
  | any isGap flags = Just $ head $ filter isGap flags
  | otherwise       = Nothing

nrOfErrors :: [Flag] -> Maybe Flag
nrOfErrors flags
  | any isNrOfErrors flags = Just $ head $ filter isNrOfErrors flags
  | otherwise              = Nothing

isLengthInBetween :: [Flag] -> Bool
isLengthInBetween flags  =  length flags == 2
                         && any isLengthAtLeast flags 
                         && any isLengthAtMost  flags 
      
getLengthAtLeast :: Flag -> Int
getLengthAtLeast (LengthAtLeast n)  =  n 
getLengthAtLeast _                  =  error "No length at least specified"

getLengthAtLeasts :: [Flag] -> Int
getLengthAtLeasts flags = case filter isLengthAtLeast flags of
                            [lal]  ->  getLengthAtLeast lal
                            _      ->  error "No or too many length at least specified"

getLengthAtMost :: Flag -> Int
getLengthAtMost (LengthAtMost n)    =  n 
getLengthAtMost _                   =  error "No length at most specified"

getLengthAtMosts :: [Flag] -> Int
getLengthAtMosts flags = case filter isLengthAtMost flags of
                           [lal]  ->  getLengthAtMost lal
                           _      ->  error "No or too many length at least specified"

handleOptions :: [Flag] -> (B.ByteString -> String,Bool)
handleOptions flags = 
  (dispatchFlags 
     (palindromeVariant   flags)
     (outputFormat        flags)
     (algorithmComplexity flags)
     (lengthModifier      flags)
     (gap                 flags)
     (nrOfErrors          flags)
  ,any isStandardInput flags
  )

dispatchFlags :: Maybe Flag -> Maybe Flag -> Maybe Flag -> Maybe Flag -> Maybe Flag -> Maybe Flag -> B.ByteString -> String
dispatchFlags pvariant out ac l g k = case pvariant of
  Nothing            ->  const (usageInfo headerHelpMessage options)
  Just Help          ->  const (usageInfo headerHelpMessage options)
  Just (Extend c)    ->  dnaLengthGappedApproximatePalindromeAround g k c
  _                  ->  palindrome pvariant out ac l g k

headerHelpMessage :: String
headerHelpMessage = 
     "*********************\n"
  ++ "* Palindrome Finder *\n"
  ++ "* version 0.4       *\n"
  ++ "*********************\n"
  ++ "Usage:"
