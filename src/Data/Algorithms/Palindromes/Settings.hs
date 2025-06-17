{-# LANGUAGE BangPatterns #-}

{- |
Module      :  Data.Algorithms.Palindromes.Settings
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences)

Describes the settings for the palindrome finder functions.
Functions that are used to get settings,
and therefore also functions that apply settings to go from input to output are also described here.
-}
module Data.Algorithms.Palindromes.Settings
    ( Settings (..)
    , getSettings
    , defaultSettings
    , handleFlags
    )
where

import Data.List (intercalate)
import System.Console.GetOpt (usageInfo)

import Data.Algorithms.Palindromes.Finders
    ( Algorithm (..)
    , OutputFilter (..)
    , OutputFormat (..)
    , Variant (..)
    , findPalindromesFormatted
    , formatPalindromes
    )
import Data.Algorithms.Palindromes.Options
    ( Flag
    , defaultAlgorithm
    , defaultMinLength
    , defaultOutputFilter
    , defaultOutputFormat
    , defaultVariant
    , getAlgorithm
    , getMinLength
    , getOutputFilter
    , getOutputFormat
    , getProgressDisabled
    , getVariant
    , headerHelpMessage
    , isHelp
    , isStandardInput
    , options
    )
import Data.Algorithms.Palindromes.Streaming (findPalindromesWithProgressBar)

-- | Data type with all the settings required for running algorithm.
data Settings = Settings
    { algorithm :: Algorithm
    , variant :: Variant
    , outputFormat :: OutputFormat
    , outputFilter :: OutputFilter
    , minLength :: Int
    }

instance Show Settings where
    show settings = intercalate ", " settingsSpecs
      where
        settingsSpecs =
            [ show (algorithm settings)
            , show (variant settings)
            , show (outputFormat settings)
            , show (outputFilter settings)
            , show (minLength settings)
            ]

-- | If no flags are given to modify settings default settings are used
defaultSettings :: Settings
defaultSettings =
    Settings
        { algorithm = defaultAlgorithm
        , variant = defaultVariant
        , outputFormat = defaultOutputFormat
        , outputFilter = defaultOutputFilter
        , minLength = defaultMinLength
        }

{- | Gets settings from the list of input flags. Uses default if no flags are given.
Evaluates all Settings records to WHNF using bang patterns to ensure all flags are valid
before calculation begins
-}
getSettings :: [Flag] -> Settings
getSettings flags =
    Settings
        { algorithm = alg
        , variant = var
        , outputFormat = fmt
        , outputFilter = flt
        , minLength = minL
        }
  where
    !alg = getAlgorithm flags
    !var = getVariant flags
    !fmt = getOutputFormat flags
    !flt = getOutputFilter flags
    !minL = getMinLength flags

-- | Finds all formatted palindromes given the settings. Can be done with and without a progress bar.
applySettingsToFinder
    :: Bool
    -- ^ Is the progress bar disabled
    -> Settings
    -- ^ The settings to find palindromes with
    -> (String -> IO String)
applySettingsToFinder
    progressDisabled
    ( Settings
            { algorithm = c
            , variant = v
            , outputFormat = o
            , outputFilter = f
            , minLength = l
            }
        )
    input
        | progressDisabled = return $ findPalindromesFormatted v o f c l input
        | otherwise = do
            pals <- findPalindromesWithProgressBar v c l filterOnlyLongest input
            return (formatPalindromes o pals)
      where
        filterOnlyLongest = case f of
            SelectLongest -> True
            _ -> False

{- | Based on input flags, gets a tuple with a function that directly encapsulates
everything from the input string to the output string. Also encodes whether input string
is from a file or standard input.
-}
handleFlags
    :: [Flag]
    -> Bool
    -> ( String -> IO String -- function from input to output
       , Bool -- if input is standard input
       )
handleFlags flags hasFiles =
    ( if any isHelp flags || (null flags && not hasFiles)
        then const $ return (usageInfo headerHelpMessage options)
        else applySettingsToFinder progressDisabled (getSettings flags)
    , any isStandardInput flags
    )
  where
    progressDisabled = getProgressDisabled flags
