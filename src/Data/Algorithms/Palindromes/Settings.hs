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
module Data.Algorithms.Palindromes.Settings (Settings (..))
where

import Data.List (intercalate)

import Data.Algorithms.Palindromes.Finders
    ( Algorithm (..)
    , OutputFilter (..)
    , OutputFormat (..)
    , Variant (..)
    )

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
