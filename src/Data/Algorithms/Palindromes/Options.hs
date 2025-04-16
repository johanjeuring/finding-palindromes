{-# LANGUAGE PatternGuards #-}
-- Did not yet translate all options. Complete the table in dispatchFlags.
-- Default doesn't work yet
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- |
Module      :  Data.Algorithms.Palindromes.Options
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences)

Gives the options for flags that can be input in the command line.
Also contains the functions that are used to convert these flags to their corresponding datatype.
-}
module Data.Algorithms.Palindromes.Options where

import Data.Maybe (fromJust, isNothing)
import System.Console.GetOpt
    ( ArgDescr (..)
    , OptDescr (..)
    )

import Data.Algorithms.Palindromes.Finders
    ( Complexity (..)
    , LengthMod
    , OutputFormat (..)
    , Variant (..)
    )

data Flag
    = Help
    | StandardInput
    | Complexity Complexity
    | Variant Variant
    | OutputFormat OutputFormat
    | LengthMod LengthMod
    | MinLength Int
    | MaxLength Int

defaultComplexity :: Complexity
defaultComplexity = ComQuadratic{gapSize = 0, maxError = 0}

defaultVariant :: Variant
defaultVariant = VarText

defaultOutputFormat :: OutputFormat
defaultOutputFormat = OutWord

defaultLengthMod :: LengthMod
defaultLengthMod = (0, Nothing)

-----------------------------------------------------------------------------
-- Options
-----------------------------------------------------------------------------

{- Options describe the input flags that can be used in the command line. Only single
letter options are used (except "help"), because if a letter within a multiple letter
option is recognized, it is taken as a single letter option. -}
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
        "u"
        []
        (NoArg (Variant VarPunctuation))
        "Palindrome surrounded by punctuation (if any)"
    , Option
        "w"
        []
        (NoArg (Variant VarWord))
        "Word palindrome"
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

-- | Detects help flag constructor.
isHelp :: Flag -> Bool
isHelp Help = True
isHelp _ = False

-- | Detects standard input flag constructor.
isStandardInput :: Flag -> Bool
isStandardInput StandardInput = True
isStandardInput _ = False

{- | Parses the optional error and gap input to a Flag. If invalid inputs are given, an
error is thrown.
-}
parseQuadratic :: Maybe String -> Flag
parseQuadratic str
    | isNothing str = Complexity ComQuadratic{gapSize = 0, maxError = 0}
    | null y =
        error
            ( "Invalid arguments for gapsize and errors. (gapsize, errors) = ("
                ++ fst nums
                ++ ", "
                ++ snd nums
                ++ "). q must be the last flag in a series of flags."
                ++ " Enter 2 numbers after q seperated by a '+'. For example: '-q1+2'."
            )
    | otherwise =
        Complexity ComQuadratic{gapSize = read (fst nums), maxError = read (snd nums)}
  where
    (x, y) = break (== '+') $ fromJust str
    nums = (x, drop 1 y)

{- | From all input flags, gets the complexity setting. If more than one complexity flag
is given, it throws an error, as this is not suppported by our program. If none are give it
uses the default option.
-}
getComplexity :: [Flag] -> Complexity
getComplexity xs
    | null complexityFlags = defaultComplexity
    | [Complexity c] <- complexityFlags = c
    | otherwise = error "Multiple complexity flags detected."
  where
    isComplexity :: Flag -> Bool
    isComplexity (Complexity _) = True
    isComplexity _ = False
    complexityFlags :: [Flag]
    complexityFlags = filter isComplexity xs

{- | From all input flags, gets the variant setting. If more than one variant flag is
given, it throws an error, as this is not suppported by our program. If none are give it
uses the default option.
-}
getVariant :: [Flag] -> Variant
getVariant xs
    | null variantFlags = defaultVariant
    | [Variant v] <- variantFlags = v
    | otherwise = error "Multiple variant flags detected."
  where
    isVariant :: Flag -> Bool
    isVariant (Variant _) = True
    isVariant _ = False
    variantFlags :: [Flag]
    variantFlags = filter isVariant xs

{- | From all input flags, gets the output format setting. If more than one output format
flag is given, it throws an error, as this is not suppported by our program. If none are
give it uses the default option.
-}
getOutputFormat :: [Flag] -> OutputFormat
getOutputFormat xs
    | null outputFormatFlags = defaultOutputFormat
    | [OutputFormat o] <- outputFormatFlags = o
    | otherwise = error "Multiple outputFormat flags detected."
  where
    isOutputFormat :: Flag -> Bool
    isOutputFormat (OutputFormat _) = True
    isOutputFormat _ = False
    outputFormatFlags :: [Flag]
    outputFormatFlags = filter isOutputFormat xs

{- | From all input flags, gets the length modifier setting. If more than one length
modifier flag is given, it throws an error, as this is not suppported by our program. If
none are give it uses the default option.
-}
getLengthMod :: [Flag] -> LengthMod
getLengthMod xs = (minLength, maxLength)
  where
    isMinLength (MinLength _) = True
    isMinLength _ = False
    isMaxLength (MaxLength _) = True
    isMaxLength _ = False
    mins :: [Flag]
    mins = filter isMinLength xs
    maxs :: [Flag]
    maxs = filter isMaxLength xs
    minLength :: Int
    minLength
        | null mins = 0
        | [MinLength minL] <- mins = minL
        | otherwise = error "Multiple minimum lengths found."
    maxLength :: Maybe Int
    maxLength
        | null maxs = Nothing
        | [MaxLength maxL] <- maxs = Just maxL
        | otherwise = error "Multiple maximum lengths found."

-- | The header of the help message.
headerHelpMessage :: String
headerHelpMessage =
    "*********************\n"
        ++ "* Palindrome Finder *\n"
        ++ "* version 0.5       *\n"
        ++ "*********************\n"
        ++ "Usage:"
