{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      :  Data.Algorithms.Palindromes.PalEq
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  provisional
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

This module describes the class PalEq. This is equality but for palindromes. For many
types with an instance of equality this can be used as such. Some types however such as
DNA, where we want characters to match to other characters we create our own instance.
This generalizes equal to a "palindromic equals".
-}
module Data.Algorithms.Palindromes.PalEq
    ( PalEq (..)
    , palEqToItselfAtIndex
    ) where

import qualified Data.Vector.Generic as G

{- | (=:=) determines whether or not two elements are equal when finding palindromes.
This is not always standard equality for example, A (=:=) T in DNA, and 'z' (=:=) 'z' in normal text.
-}
class PalEq a where
    (=:=) :: a -> a -> Bool

-- | Define PalEq instance for any a of class Eq. Just use the equality relation. If it is different it can be overriden.
instance (Eq a) => PalEq a where
    (=:=) = (==)

{- | Safe function which returns whether an element at an index in the input vector is
PalEq to itself.
-}
palEqToItselfAtIndex :: (PalEq a, G.Vector v a) => v a -> Int -> Bool
palEqToItselfAtIndex input index
    | index < 0 || index >= G.length input = False
    | otherwise = element =:= element
  where
    element = input G.! index
