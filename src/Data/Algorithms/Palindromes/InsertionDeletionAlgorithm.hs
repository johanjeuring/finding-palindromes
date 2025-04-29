{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module Data.Algorithms.Palindromes.InsertionDeletionAlgorithm where

import Data.Bifunctor (second)

import Data.Algorithms.Palindromes.PalEq (PalEq, (=:=))

import qualified Data.Vector as V

-- | Represents cell location in the matrix. Format is: (row, colunm)
type Position = (Int, Int)

-- | Represents a cell in the matrix. The format is: ((row, column), value)
data Cell = Cell
    { cellPosition :: Position
    , cellValue :: Int
    }
    deriving (Show)

-- | Represents a row in the matrix.
type Row = [Cell]

{- Represents a maximal palindrome found by the matrix.
The format is: (leftBound (inclusive), rightBound (exclusive), length)-}
type MaxPal = (Int, Int, Int)

insertionDeletionIteration
    :: (PalEq a)
    => V.Vector a
    -- ^ Input vector
    -> Int
    -- ^ Maximum number of errors
    -> Int
    -- ^ Index of current row
    -> ([[(Int, Int, Int)]], Row)
    -> ([[(Int, Int, Int)]], Row)
insertionDeletionIteration input maxErrors rowIndex (founds, prevRow) = undefined

-- make sure sparsify happens after extractMaximalPalindromes

{- Calculates the position of the maximal palindromes in the row below the current row.
It does for the previous since you need the current row to know if the budget is exceeded when the palindrome is extended.
If it is not the palindrome is obviously not maximal.
-}
extractMaximalPalindromes
    :: [(Cell, Int)]
    -- ^ Current row that has the budget of previous row
    -> [MaxPal]
    -- ^ The maximal palindromes found
extractMaximalPalindromes =
    map apos
        {- Filters positions that do not exceed budget but full triangle to the top right does,
        using the position and budgets from scanr.
        These are maximal since they cannot be extended without exceeding the budget.
        -}
        . filter isMaximal
        {- Fills list of same size as the current row with tuple
        (position, (budget above, budget at position), (budget diag, budget right))
        Starts accumulator out of bounds with negative budgets since only able to extend out of bounds makes maximal.
        -}
        . scanr nextMatrix (undefined, (-1, -1), (-1, -1))
  where
    -- Format to maxPal type
    -- look at why col + 1??
    apos ((row, col), _, _) = (col + 1, row, col - row)
    nextMatrix
        :: (Cell, Int)
        -- \^ Cell is at the position above the one we want the matrix from, the int is the budget at the position
        -> (Position, (Int, Int), (Int, Int))
        -> (Position, (Int, Int), (Int, Int))
    {- Since the scanr is done over current row we add 1 to row since we want position at previous row.
    The value in the cell of the row is the current so above the previous row. -}
    nextMatrix (Cell (row, col) budgetAbove, budget) (_, (budgetRight, budgetDiag), _) = ((row + 1, col), (budgetAbove, budget), (budgetDiag, budgetRight))

isMaximal :: (Position, (Int, Int), (Int, Int)) -> Bool -- Why do we need to check x > y+1?
isMaximal ((x, y), (a, c), (b, d)) = c >= 0 && a < 0 && b < 0 && d < 0 && x > y + 1

{- Fills a row in the matrix from left to right.
-}
fillRow
    :: (PalEq a)
    => V.Vector a
    -- ^ Input vector
    -> Int
    -- ^ Maximum number of errors
    -> Int
    -- ^ Index of current row
    -> Row
    -- ^ Previous row
    -> [(Cell, Int)]
    -- ^ Current row that has the budget of previous row (the budgets in the cells below)
fillRow input maxErrors rowIndex = scanl getNextBudget (leftMostCell, maxErrors)
  where
    -- The leftmost cell of the row to fill. It always has full budget.
    leftMostCell = Cell (rowIndex, rowIndex) maxErrors
    getNextBudget :: (Cell, Int) -> Cell -> (Cell, Int)
    getNextBudget (Cell prevPosition valLeft, valDiagonal) (Cell _ valBelow) =
        (Cell currentPosition bestBudget, valBelow)
      where
        -- taking left or right budget has error cost of 1 for an insertion
        budgetFromLeft = valLeft - 1
        budgetFromBelow = valBelow - 1
        budgetFromDiagonal = valDiagonal - errorCostAtPosition currentPosition
        -- prevPosition is one column to the left of the current position
        currentPosition = second (+ 1) prevPosition
        bestBudget = maximum [budgetFromLeft, budgetFromBelow, budgetFromDiagonal]

    -- if elements are palindrome equal at position then no error cost, otherwise error cost of 1 for substitution
    errorCostAtPosition :: Position -> Int
    errorCostAtPosition (row, column) = if (input V.! row) =:= (input V.! column) then 0 else 1

sparsify :: Row -> Row
sparsify denseRow = sparseRow
  where
    positives = filter (\(Cell _ budget) -> budget >= 0) denseRow

    sparseRow :: Row
    sparseRow =
        case positives of
            [] -> []
            ((Cell (row, _) _) : _) -> snd $ foldr addMinusOnes ((row, length denseRow), []) positives

    addMinusOnes :: Cell -> (Position, Row) -> (Position, Row)
    addMinusOnes cell@(Cell (row, col) budget) (prevPos@(prevRow, prevCol), acc)
        -- if positive cells are next to each other, do nothing
        | abs (col - prevCol) == 1 = (cellPosition cell, cell : acc)
        -- if one position between positive cells, add one cell with -1 budget
        | abs (col - prevCol) == 2 = (cellPosition cell, cell : Cell (row, col + 1) (-1) : acc)
        -- if more than one position between positive cells, add cells with -1 budget on edges of the gap between them
        | otherwise =
            ( cellPosition cell
            , cell : Cell (row, col + 1) (-1) : Cell (prevRow, prevCol - 1) (-1) : acc
            )

-- -- assume first element is positive
-- keepPositive :: Row -> Row
-- keepPositive [] = []
-- kepPositive row = let (positives, tail) = span (>= 0) row in positives ++ ((-1) : dropNegative newRow)

-- dropNegative :: Row -> Row
-- dropNegative [] = []
-- dropNegative row = let newRow = dropWhile (< 0) row in keepPositive newRow
