{- |
Module      :  Data.Algorithms.Palindromes.Settings
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

Describes the settings for the palindrome finder functions.
-}
module Data.Algorithms.Palindromes.Settings (Settings (..), applySettingsToFinder)
where

import Data.List (intercalate)

import Data.Algorithms.Palindromes.Finders
    ( Algorithm (..)
    , OutputFilter (..)
    , OutputFormat (..)
    , Variant (..)
    , findPalindromesFormatted
    , formatPalindromes
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
