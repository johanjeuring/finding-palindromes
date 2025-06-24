{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      :  Data.Algorithms.Palindromes.DNA
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  provisional
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

This module contains the DNA datatype and corresponding functions.
This type encodes a representation for DNA that can be used as input for finding
palindromes.
-}
module Data.Algorithms.Palindromes.DNA where

import Data.Char (toUpper)
import Data.Maybe (fromJust, isNothing)
import Data.Word (Word8)

import Data.Algorithms.Palindromes.PalEq (PalEq (..))

import qualified Data.Vector.Generic as G (Vector (..))
import qualified Data.Vector.Generic.Mutable as GM (MVector (..))
import qualified Data.Vector.Unboxed as U (MVector, Unbox, Vector, any, map)

{- | Datatype for the different DNA bases, note that (=)/Eq is not suitable for checking if DNA
  has palindromes, instead PalEq should be used. This is because DNA is anti-reflexive,
   meaning it will never match with itself.
-}
data DNA = A | T | C | G | N deriving (Show, Eq, Enum)

newtype instance U.MVector s DNA = MV_DNA (U.MVector s Word8)
newtype instance U.Vector DNA = V_DNA (U.Vector Word8)

instance GM.MVector U.MVector DNA where
    {-# INLINE basicLength #-}
    basicLength (MV_DNA v) = GM.basicLength v
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice a b (MV_DNA v) = MV_DNA $ GM.basicUnsafeSlice a b v
    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_DNA a) (MV_DNA b) = GM.basicOverlaps a b
    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew i = MV_DNA <$> GM.basicUnsafeNew i
    {-# INLINE basicInitialize #-}
    basicInitialize (MV_DNA v) = GM.basicInitialize v
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_DNA v) i = toEnum . fromIntegral <$> GM.basicUnsafeRead v i
    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_DNA v) i = GM.basicUnsafeWrite v i . fromIntegral . fromEnum

instance G.Vector U.Vector DNA where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_DNA v) = V_DNA <$> G.basicUnsafeFreeze v
    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_DNA v) = MV_DNA <$> G.basicUnsafeThaw v
    {-# INLINE basicLength #-}
    basicLength (V_DNA v) = G.basicLength v
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice a b (V_DNA v) = V_DNA $ G.basicUnsafeSlice a b v
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_DNA v) i = toEnum . fromIntegral <$> G.basicUnsafeIndexM v i

instance U.Unbox DNA

-- | Defined as the base pairs for DNA. Meaning A matches T and G matches with C.
instance {-# OVERLAPPING #-} PalEq DNA where
    A =:= T = True
    T =:= A = True
    G =:= C = True
    C =:= G = True
    _ =:= _ = False

{- | Parses an unboxed vector with chars to an unboxed vector with the DNA datatype.
Returns Nothing if the input cannot be fully parsed to DNA.
-}
toDNA :: U.Vector Char -> Maybe (U.Vector DNA)
toDNA x
    | hasNothing = Nothing
    | otherwise = Just $ U.map (fromJust . charToDNA) x
  where
    hasNothing = U.any (isNothing . charToDNA) x

-- | Parses a single Char to the DNA datatype. Returns Nothing if this is not possible.
charToDNA :: Char -> Maybe DNA
charToDNA 'A' = Just A
charToDNA 'T' = Just T
charToDNA 'C' = Just C
charToDNA 'G' = Just G
charToDNA 'U' = Just T
charToDNA 'a' = Just A
charToDNA 't' = Just T
charToDNA 'c' = Just C
charToDNA 'g' = Just G
charToDNA 'u' = Just T
charToDNA c
    | toUpper c `elem` "RYKMSWBDHVN" = Just N
    | otherwise = Nothing

-- | Converts the DNA datatype to the corresponding Char symbol
dnaToChar :: DNA -> Char
dnaToChar A = 'A'
dnaToChar T = 'T'
dnaToChar G = 'G'
dnaToChar C = 'C'
dnaToChar N = 'N'
