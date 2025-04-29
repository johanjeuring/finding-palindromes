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
sparsify row = go [] row Nothing
  where
    go
        :: Row --  Accumulator. Contains the sparsified row thus far in reverse order.
        -> Row -- The rest of the dense row which must still be sparsified.
        -> Maybe Cell -- The last found cell with a non-negative budget.
        -> Row -- The final sparsified row.
        -- row has been completed, return the accumulator, which is now in reverse
    go acc [] _ = reverse acc
    -- while no cell with positive budget is found, start main loop if you find one, else go to next cell
    go acc (cell@(Cell pos val) : rest) Nothing
        | val >= 0 = go (cell : acc) rest (Just cell)
        | otherwise = go acc rest Nothing
    -- the main loop, keep filling accumulator and ignoring the strings of cells with negative budgets
    go acc (cell@(Cell (r, c) val) : rest) (Just prevCell@(Cell (prevR, prevC) _))
        | val >= 0 =
            case c - prevC of
                -- just add the cell to accumulator
                1 -> go (cell : acc) rest (Just cell)
                -- Add one cell with a (-1) budget in the gap
                2 -> go (cell : Cell (r, c - 1) (-1) : acc) rest (Just cell)
                -- Add two cells with (-1) budget at the edges of the gap
                _ -> go (cell : Cell (r, c - 1) (-1) : Cell (prevR, prevC + 1) (-1) : acc) rest (Just cell)
        | otherwise = go acc rest (Just prevCell)
