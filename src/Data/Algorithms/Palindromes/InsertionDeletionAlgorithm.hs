{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

{- |
Module      :  Data.Algorithms.Palindromes.Palindrome
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences)

The insertion deletion algorithm for maximal gapped approximate palindromes with a maximum
number of insertion, deletion and substitution errors.
-}
module Data.Algorithms.Palindromes.InsertionDeletionAlgorithm (Cell (..), insertionDeletionAlgorithm, sparsify) where

import Data.Algorithms.Palindromes.PalEq (PalEq, (=:=))

import qualified Data.Vector.Generic as G

{- | Represents the range of the substring of the input string containing a palindrome.
Format: ([start index (inclusive)], [end index (exclusive)]).
-}
type PalRange = (Int, Int)

{- | Represents a cell in the matrix. Each cell corresponds to some substring of the input
string. The row is the index of the first character (inclusive) and the column is the
index of the last character (inclusive) of the substring.
-}
data Cell = Cell
    { cellColumn :: Int
    , cellBudget :: Budget
    }
    deriving (Show, Eq)

-- | Represents a row in the matrix.
type Row = [Cell]

{- | The budget of a cell is the number of errors that can be added to the substring the
cell corresponds to without exceeding the maximum number of errors.
-}
type Budget = Int

{- | Find all maximal gapped approximate palindromes with a certain gap and a certain
maximum number of errors.
-}
insertionDeletionAlgorithm
    :: (PalEq a, G.Vector v a)
    => Int
    -- ^ The size of the gap
    -> Int
    -- ^ The maximum number of errors
    -> v a
    -- ^ The input vector
    -> [PalRange]
    -- ^ The list of found maximal gapped approximate palindromes
insertionDeletionAlgorithm gapSize maxErrors input = concatMap (\(_, palRanges, _) -> palRanges) states
  where
    -- Bound the used gapSize to not be more than the length of the input.
    gapSize' = min gapSize (G.length input)
    -- Use takeIterations to get the states for efficiency.
    states = takeIterations nrOfIterations (fillRow input gapSize' maxErrors) startState
    {- Required number of iterations is (+ 2) to also be able to spot maximal palindromes
    in the two upper rows. -}
    nrOfIterations = maxRow + 2
    startState =
        (
            [ Cell
                { {- We start the matrix on the bottom right, so at the last index of the
                  input. -}
                  cellColumn = lastIndex
                , -- This cell corresponds to an empty substring, so it has no errors.
                  cellBudget = maxErrors
                }
            ] -- This defines the entire bottom row of the matrix.
        , [] -- no maximal palindromes found yet.
        , maxRow - 1 -- row number of the row above the bottom row of the matrix.
        )
    lastIndex = G.length input - 1
    -- The index of the last row, adjusted with the gap size to ignore errors in the gap.
    maxRow = lastIndex - gapSize' + 1

-- | Fills the next row and finds maximal palindromes in the previous row
fillRow
    :: (PalEq a, G.Vector v a)
    => v a
    -- ^ Input vector
    -> Int
    -- ^ Maximum size of the gap
    -> Int
    -- ^ Maximum number of errors
    -> (Row, [PalRange], Int)
    -- ^ Old state
    -> (Row, [PalRange], Int)
    -- ^ New state
fillRow input gapSize maxErrors (row, _, rowIndex) =
    (firstCell ++ newRow, foundMaxPals, rowIndex - 1)
  where
    {- The first cell of the current row. A new cell needs to be added at
    the start of this row because we don't add one in the evaluatePosition part
    and every row has one more cell to the left than the previous row. -}
    firstCell
        | initialColumn >= 0 = [Cell{cellColumn = initialColumn, cellBudget = initialBudget}]
        | otherwise = []

    -- The initial column is directly to the left of the diagonal on this row.
    initialColumn = rowIndex + gapSize - 1

    -- The budget of the first cell of this row
    initialBudget :: Budget
    initialBudget
        {- For rows or columns out of bounds, the first cell of the previous row has no
        budget left, because it represents an invalid substring. -}
        | rowIndex < 0 || initialColumn >= G.length input = -1
        {- In general, the first cell of the previous row has a budget of maxErrors,
        because it represent an empty substring, which has no errors. -}
        | otherwise =
            maxErrors

    -- Sparsify the previous row.
    sparsePrevRow =
        sparsify row

    {- EvaluatePosition generates tuples with the cell at a position as the
    second element and in the third position the cell to the bottomleft of the evaluated position as a found
    gapped approximate maximal palindrome if it is one. We concat map these so we only traverse once,
    by compiler optimisation. We thus find the new row and the the updated found palindromes.
    -}
    scannedRow =
        drop 1 $
            scanl (evaluatePosition input rowIndex) ((maxErrors, maxErrors), [], []) sparsePrevRow
    newRow = concatMap (\(_, rowSegment, _) -> rowSegment) scannedRow
    foundMaxPals = concatMap (\(_, _, palRanges) -> palRanges) scannedRow

{- | Define a new cell (the cell above the input cell) with the correct budget and add
bottom left cell to maxPals if it is maximal.

Consider the following matrix:

@
------------------------------
| topLeft        topRight    |
| bottomLeft     bottomRight |
------------------------------
@

We want to define the budget in topRight and check whether bottomLeft is maximal.
-}
evaluatePosition
    :: (PalEq a, G.Vector v a)
    => v a
    -- ^ Input vector
    -> Int
    -- ^ row index
    -> ((Budget, Budget), Row, [PalRange])
    -- ^ Old state
    -> Cell
    -- ^ Input cell
    -> ((Budget, Budget), Row, [PalRange])
    -- ^ New state
evaluatePosition input rowIndex ((topLeft, bottomLeft), _, _) (Cell{cellColumn = column, cellBudget = bottomRight}) =
    ((topRight, bottomRight), [Cell column topRight], maxpals)
  where
    topRight
        | rowIndex >= 0 && column < G.length input =
            maximum
                [ topLeft - 1
                , bottomRight - 1
                , bottomLeft - errorCostAtPosition input (rowIndex, column)
                ]
        | otherwise = -1
    maxpals
        {- add (+ 1) to get inclusive start index and exclusive end index for the found
        maximal palindromes -}
        | bottomLeft >= 0 && topLeft < 0 && topRight < 0 && bottomRight < 0 =
            [(rowIndex + 1, column)]
        | otherwise = []

{- | Replace long sequences of cells with (-1) budgets with two cells with (-1) budgets,
one on either end of the sequence.
-}
sparsify :: Row -> Row
sparsify [] = []
sparsify row@(Cell{cellColumn = firstColumnIndex, cellBudget = _} : _) = sparseRow
  where
    -- due to haskells lazy evaluation this only traverses the list once.
    sparseRow =
        -- Extracts the sparsified row by concatmapping all the second elements of the accumulator
        concatMapWithEndFunction snd getFinalCell $
            -- We filter the negatives from the row and add back negatives at positions at the
            scanl insertNegatives (firstColumnIndex, []) (filter ((>= 0) . cellBudget) row)

    insertNegatives :: (Int, Row) -> Cell -> (Int, Row)
    insertNegatives (lastind, _) newCell@Cell{cellColumn = newIndex}
        -- Place two cells on either side of the sequence of negative budgets.
        | newIndex - lastind > 2 =
            ( newIndex
            ,
                [ Cell{cellColumn = lastind + 1, cellBudget = -1}
                , Cell{cellColumn = newIndex - 1, cellBudget = -1}
                , newCell
                ]
            )
        -- Add one cell with (-1) budget back after it has apparently been filtered out before.
        | newIndex - lastind == 2 =
            (newIndex, [Cell{cellColumn = lastind + 1, cellBudget = -1}, newCell])
        -- Do nothing.
        | otherwise = (newIndex, [newCell])

    -- We always must add one -1 to the end of the sparsified row
    getFinalCell (lastPositiveColumnIndex, _) =
        [Cell{cellColumn = lastPositiveColumnIndex + 1, cellBudget = -1}]

    -- concatMap but we also apply an "end function" to the final element
    concatMapWithEndFunction _ _ [] = []
    concatMapWithEndFunction f endf [s] = f s ++ endf s
    concatMapWithEndFunction f endf (s : x : t) =
        f s
            ++ concatMapWithEndFunction
                f
                endf
                (x : t)

-- | Returns first n elements of iterate f on start.
takeIterations :: Int -> (a -> a) -> a -> [a]
takeIterations n f start = take n $ iterate f start

{- If elements are palindrome equal at position then no error cost, otherwise error cost
of 1 for a substitution error. -}
errorCostAtPosition :: (PalEq a, G.Vector v a) => v a -> (Int, Int) -> Int
errorCostAtPosition input (row, column)
    | (input G.! row) =:= (input G.! column) = 0
    | otherwise = 1
