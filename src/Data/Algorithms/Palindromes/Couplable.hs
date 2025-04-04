-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- \|
-- Module      :  Data.Algorithms.Palindromes.Couplable
-- Copyright   :  (c) 2007 - 2013 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Algorithms.Palindromes.Couplable
    ( Couplable (..)
    , couplableWithItselfAtIndex
    ) where

import Data.Vector as V

{- |
  Shows that some element belongs to another element.
  For example, A belongs to T in DNA, and 'z' belongs to 'z' in normal text.
-}
class Couplable a where
    (=:=) :: a -> a -> Bool

-- | Define Couplable instance for any a of class Eq. Just use the equality relation.
instance (Eq a) => Couplable a where
    (=:=) = (==)

{- | Safe function which returns whether an element at an index in the input vector is
  couplable with itself.
-}
couplableWithItselfAtIndex :: (Couplable a) => V.Vector a -> Int -> Bool
couplableWithItselfAtIndex input index
    | index < 0 || index >= V.length input = False
    | otherwise = element =:= element
  where
    element = input V.! index