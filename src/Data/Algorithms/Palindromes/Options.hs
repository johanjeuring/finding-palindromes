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
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Data.Algorithms.Palindromes.Options where

import Data.Algorithms.Palindromes.Combinators
    ( Complexity (ComLinear, ComQuadratic, gapSize, maxError)
    , LengthMod
    , Output (..)
    , Variant (..)
    )
import Data.Maybe (fromJust, isNothing)
import System.Console.GetOpt
    ( ArgDescr (..)
    , OptDescr (..)
    )

data Flag
    = Help
    | StandardInput
    | Complexity Complexity
    | Variant Variant
    | OutputFormat Output
    | LengthMod LengthMod
    | MinLength Int
    | MaxLength Int

defaultComplexity :: Complexity
defaultComplexity = ComQuadratic{gapSize = 0, maxError = 0}

defaultVariant :: Variant
defaultVariant = VarText

defaultOutput :: Output
defaultOutput = OutWord

defaultLengthMod :: LengthMod
defaultLengthMod = (0, Nothing)

-----------------------------------------------------------------------------
-- Options
-----------------------------------------------------------------------------

-- I am using single letter options here (except for help): getOpt handles
-- options too flexible: in case a letter within a multiple letter option is
-- recognized, it is taken as a single letter option.
options :: [OptDescr Flag]
options =
    [ Option
        "h"
        ["help"]
        (NoArg Help)
        "This message"
    , Option
        "r"
        []
        (NoArg (Complexity ComLinear))
        "Use the linear algorithm"
    , Option
        "q"
        []
        (OptArg parseQuadratic "[gapSize] [errors]")
        "Use the quadratic algorithm. Optionally use the argument <gapSize> <errors>"
    , Option
        "p"
        []
        (NoArg (Variant VarPlain))
        "Plain palindrome (default)"
    , Option
        "t"
        []
        (NoArg (Variant VarText))
        "Palindrome ignoring case, spacing and punctuation"
    , Option
        "w"
        []
        (NoArg (Variant VarPunctuation))
        "Palindrome surrounded by punctuation (if any)"
    , Option
        "d"
        []
        (NoArg (Variant VarDNA))
        "DNA palindrome"
    , Option
        "l"
        []
        (NoArg (OutputFormat OutWord))
        "Longest palindrome (default)"
    , Option
        "e"
        []
        (NoArg (OutputFormat OutLength))
        "Length of the longest palindrome"
    , Option
        "m"
        []
        (NoArg (OutputFormat OutWords))
        "Maximal palindrome around each position in the input"
    , Option
        "a"
        []
        (NoArg (OutputFormat OutLengths))
        "Length of the maximal palindrome around each position in the input"
    , Option
        "b"
        []
        (ReqArg (MinLength . (read :: String -> Int)) "arg")
        "Maximal palindromes of length at least [arg]"
    , Option
        "c"
        []
        (ReqArg (MaxLength . (read :: String -> Int)) "arg")
        "Maximal palindromes (possibly cut off) of length at most [arg]"
    , Option
        "i"
        []
        (NoArg StandardInput)
        "Read input from standard input"
    , Option
        "x"
        []
        (ReqArg (OutputFormat . OutLengthAt . (read :: String -> Int)) "arg")
        "Extend a palindrome around center [arg]"
    ]

isHelp :: Flag -> Bool
isHelp Help = True
isHelp _ = False

isStandardInput :: Flag -> Bool
isStandardInput StandardInput = True
isStandardInput _ = False

parseQuadratic :: Maybe String -> Flag
parseQuadratic str
    | isNothing str = Complexity ComQuadratic{gapSize = 0, maxError = 0}
    | null y =
        error
            ( "Invalid arguments for gapsize and errors ("
                ++ fst nums
                ++ " "
                ++ snd nums
                ++ "). Enter 2 numbers seperated by a +"
            )
    | otherwise =
        Complexity ComQuadratic{gapSize = read (fst nums), maxError = read (snd nums)}
  where
    (x, y) = break (== '+') $ fromJust str
    nums = (x, drop 1 y)

-- functions to get each setting field from the input flags. If no flags are given to modify setting use default.

getComplexity :: [Flag] -> Complexity
getComplexity xs
    | null complexityFlags = defaultComplexity
    | length complexityFlags == 1 = c
    | otherwise = error "Multiple complexity flags detected."
  where
    isComplexity :: Flag -> Bool
    isComplexity (Complexity _) = True
    isComplexity _ = False
    complexityFlags :: [Flag]
    complexityFlags@[com] = filter isComplexity xs
    (Complexity c) = com

getVariant :: [Flag] -> Variant
getVariant xs
    | null variantFlags = defaultVariant
    | length variantFlags == 1 = v
    | otherwise = error "Multiple variant flags detected."
  where
    isVariant :: Flag -> Bool
    isVariant (Variant _) = True
    isVariant _ = False
    variantFlags :: [Flag]
    variantFlags@[var] = filter isVariant xs
    (Variant v) = var

getOutputFormat :: [Flag] -> Output
getOutputFormat xs
    | null outputFlags = defaultOutput
    | length outputFlags == 1 = o
    | otherwise = error "Multiple output flags detected."
  where
    isOutput :: Flag -> Bool
    isOutput (OutputFormat _) = True
    isOutput _ = False
    outputFlags :: [Flag]
    outputFlags@[out] = filter isOutput xs
    (OutputFormat o) = out

getLengthMod :: [Flag] -> LengthMod
getLengthMod xs = (minLength, maxLength)
  where
    isMinLength (MinLength _) = True
    isMinLength _ = False
    isMaxLength (MaxLength _) = True
    isMaxLength _ = False
    mins :: [Flag]
    mins@[min] = filter isMinLength xs
    maxs :: [Flag]
    maxs@[max] = filter isMaxLength xs
    minLength :: Int
    minLength
        | null mins = 0
        | length mins == 1 = minL
        | otherwise = error "Multiple minimum lengths found."
    maxLength :: Maybe Int
    maxLength
        | null maxs = Nothing
        | length maxs == 1 = Just maxL
        | otherwise = error "Multiple maximum lengths found."
    (MinLength minL) = min
    (MaxLength maxL) = max

headerHelpMessage :: String
headerHelpMessage =
    "*********************\n"
        ++ "* Palindrome Finder *\n"
        ++ "* version 0.4       *\n"
        ++ "*********************\n"
        ++ "Usage:"
