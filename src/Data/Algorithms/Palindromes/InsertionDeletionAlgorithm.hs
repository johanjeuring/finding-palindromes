{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module Data.Algorithms.Palindromes.InsertionDeletionAlgorithm (insertionDeletionAlgorithm, Cell (..), sparsify) where

import Data.Bifunctor (second)

import Data.Algorithms.Palindromes.PalEq (PalEq, (=:=))

import qualified Data.Vector as V

-- | Represents cell location in the matrix. Format is: (row, colunm)
type Position = (Int, Int)

-- | Represents a palindrome. Format is: (inclusive start, exclusive end)
type PalRange = (Int, Int)

-- | Represents the budget in a cell
type Budget = Int

-- | Represents a cell in the matrix. The format is: ((row, column), budget)
data Cell = Cell
    { cellPosition :: Position
    , cellBudget :: Budget
    }
    deriving (Show, Eq)

-- | Represents a row in the matrix.
type Row = [Cell]

{- Represents 2x2 matrix where:
((a,c),(b,d)) encodes the matrix columnwise
a b
c d
-}
type Matrix2 = ((Budget, Budget), (Budget, Budget))

insertionDeletionAlgorithm
    :: (PalEq a)
    => Int
    -- ^ Maximum number of errors
    -> V.Vector a
    -- ^ Input vector
    -> [PalRange]
    -- ^ Ranges of the maximal palindromes
insertionDeletionAlgorithm maxError input =
    -- Do (+ 1) on the end index to go from inclusive to exclusive
    concat maxPalindromes
  where
    (maxPalsWithoutFinalRow, finalRow) =
        foldr
            (insertionDeletionIteration input maxError)
            ( []
            ,
                [ Cell
                    { cellPosition = (V.length input - 1, V.length input - 1)
                    , cellBudget = maxError - errorCostAtPosition input (V.length input - 1, V.length input - 1)
                    }
                ]
            )
            [0 .. V.length input - 2]
    maxPalindromesFinalRow = extractMaximalPalindromesFinalRow finalRow
    maxPalindromes = maxPalindromesFinalRow : maxPalsWithoutFinalRow

insertionDeletionIteration
    :: (PalEq a)
    => V.Vector a
    -- ^ Input vector
    -> Int
    -- ^ Maximum number of errors
    -> Int
    -- ^ Index of current row
    -> ([[PalRange]], Row)
    -- ^ List of positions of maximal palindromes per row, and the row of the previous iteration
    -> ([[PalRange]], Row)
insertionDeletionIteration input maxErrors rowIndex (maxPals, prevRow) = (newMaxPals, newRow)
  where
    denseRow = fillRow input maxErrors rowIndex prevRow
    newMaxPals = extractMaximalPalindromes denseRow : maxPals
    newRow = sparsify (length input) (map fst denseRow)

-- | Fills a row in the matrix from left to right.
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
    -> [(Cell, Budget)]
    -- ^ Current row that has the budget of previous row (the budgets in the cells below)
fillRow input maxErrors rowIndex = scanl getNextBudget (leftMostCell, maxErrors)
  where
    {- The leftmost cell of the row to fill. It has full budget.
    The error cost at position is removed because not every datatype has garantueed single character palindromes. -}
    leftMostCell =
        Cell
            { cellPosition = (rowIndex, rowIndex)
            , cellBudget = maxErrors - errorCostAtPosition input (rowIndex, rowIndex)
            }
    {- Given previous cell, the budget of the cell below that and the cell below,
    calculate the budget for a new cell. -}
    getNextBudget :: (Cell, Budget) -> Cell -> (Cell, Budget)
    getNextBudget (Cell{cellPosition = prevPosition, cellBudget = valLeft}, valDiagonal) (Cell{cellPosition = _, cellBudget = valBelow}) =
        (Cell{cellPosition = currentPosition, cellBudget = bestBudget}, valBelow)
      where
        -- taking left or below budget has error cost of 1 for an insertion
        budgetFromLeft = valLeft - 1
        budgetFromBelow = valBelow - 1
        {- taking diagonal budget has error cost of 1 or 0, depending on whether the
        elements match at the current position -}
        budgetFromDiagonal = valDiagonal - errorCostAtPosition input currentPosition
        -- prevPosition is one column to the left of the current position
        currentPosition = second (+ 1) prevPosition
        bestBudget = maximum [budgetFromLeft, budgetFromBelow, budgetFromDiagonal]

-- if elements are palindrome equal at position then no error cost, otherwise error cost of 1 for substitution
errorCostAtPosition :: (PalEq a) => V.Vector a -> Position -> Int
errorCostAtPosition input (row, column)
    | (input V.! row) =:= (input V.! column) = 0
    | otherwise = 1

{- Calculates the position of the maximal palindromes in the row below the current row.
It does for the previous since you need the current row to know if the budget is exceeded when the palindrome is extended.
If it is not the palindrome is obviously not maximal.
-}
extractMaximalPalindromes
    :: [(Cell, Budget)]
    -- ^ Current row that has the budget of previous row
    -> [PalRange]
    -- ^ The positions representing maximal palindromes found
extractMaximalPalindromes =
    -- get position and one to column to get (inclusive, exclusive) palrange
    map (second (+ 1) . fst)
        {- Filters positions that do not exceed budget but full triangle to the top right
        does, using the position and budgets from scanr. These are maximal since they
        cannot be extended without exceeding the budget.
        -}
        . filter (isMaximal . snd)
        {- Fills list of same size as the current row with tuple (position, (budget above,
        budget at position), (budget diag, budget right)). Starts accumulator out of
        bounds with negative budgets since only able to extend out of bounds makes a
        position maximal.
        -}
        . scanr nextMatrix (undefined, ((-1, -1), (-1, -1)))
  where
    -- Gets the 2x2 matrix one position to the left of the current position.
    nextMatrix
        :: (Cell, Budget)
        -- The cell is at the position above the one we want the matrix for, the budget is at the position we consider.
        -> (Position, Matrix2)
        -> (Position, Matrix2)
    {- Since the scanr is done over current row we add 1 to row since we want position at
    previous row. -}
    nextMatrix (Cell{cellPosition = (row, col), cellBudget = budgetAbove}, budget) (_, (fstCol, _)) =
        ((row + 1, col), ((budgetAbove, budget), fstCol))

{- Matrix:
a b
c d

We want to check whether c is the position of a maximal palindrome. This is only the case
if c is non-negative and the rest of the matrix is negative.
-}
isMaximal :: Matrix2 -> Bool
isMaximal ((a, c), (b, d)) = c >= 0 && a < 0 && b < 0 && d < 0

{- For the final row, so without a row above it, a cell represents a maximal palindrome
if the cell has a positive budget and the cell to the right of it has a negative budget.
This function uses this property to find the maximal palindromes in the final row.
-}
extractMaximalPalindromesFinalRow :: Row -> [PalRange]
extractMaximalPalindromesFinalRow row = map (second (+ 1)) (go row [])
  where
    go :: Row -> [Position] -> [Position]
    go [] acc = acc
    -- The final cell is maximal only if budget is non-negative.
    go [c] acc
        | cellBudget c >= 0 = cellPosition c : acc
        | otherwise = acc
    go (c0 : c1 : cs) acc
        | cellBudget c0 >= 0 && cellBudget c1 < 0 = go cs (cellPosition c0 : acc)
        | otherwise = go (c1 : cs) acc

{- | For (large) substrings of cell with negative budgets, put one cell with (-1) budget
at each end of it. This saves unnecessary memory use.
-}
sparsify :: Int -> Row -> Row
sparsify _ [] = []
sparsify inputLength (firstCell : cells) = reverse (extraCell ++ sparsifiedReversed)
  where
    -- remove negative budgets from the tail
    filteredRow = firstCell : filter ((>= 0) . cellBudget) cells

    -- At the start and end of removed sequence of negatives in the filtered row inserts negative cells.
    (lastCell, sparsifiedReversed) = foldl insertNegatives (firstCell, []) filteredRow

    {- Before a cell is added to the sparsifyReversed list inserts a negative cell on both sides of a gap
    when there is one. Based on the position of last positive budget cell and the current cell.
    -}
    insertNegatives :: (Cell, Row) -> Cell -> (Cell, Row)
    insertNegatives (Cell{cellPosition = (_, prevC), cellBudget = _}, acc) newCell@(Cell{cellPosition = (r, c), cellBudget = _})
        | c - prevC > 2 =
            ( newCell
            , newCell
                : Cell{cellPosition = (r, c - 1), cellBudget = -1}
                : Cell{cellPosition = (r, prevC + 1), cellBudget = -1}
                : acc
            )
        | c - prevC == 2 =
            (newCell, newCell : Cell{cellPosition = (r, prevC + 1), cellBudget = -1} : acc)
        | otherwise = (newCell, newCell : acc)

    -- We need a negative cell at the end of the row, unless this is out of bounds of the input.
    extraCell = [Cell{cellPosition = (lastR, lastC + 1), cellBudget = -1} | lastC < inputLength - 1]
      where
        (Cell{cellPosition = (lastR, lastC), cellBudget = _}) = lastCell
