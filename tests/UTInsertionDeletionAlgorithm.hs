module UTLinearAlgorithm (testListInsertionDeletionAlgorithm) where

import InsertionDeletionAlgorithm (Cell, sparsify)
import Test.HUnit (Test (..), assertEqual)

testListInsertionDeletionAlgorithm = [testSparsifySimple, testSparsifyEdgeCase, testSparsifyComplex]

testSparsifySimple :: Test
testSparsifySimple =
    TestCase $
        assertEqual
            "testSparsifySimple"
            ( sparsify
                [ Cell (0, 0) 1
                , Cell (0, 1) 0
                , Cell (0, 2) (-1)
                , Cell (0, 3) (-1)
                , Cell (0, 4) (-1)
                , Cell (0, 5) (-1)
                , Cell (0, 6) 0
                ]
            )
            [Cell (0, 0) 1, Cell (0, 1) 0, Cell (0, 2) (-1), Cell (0, 5) (-1), Cell (0, 6) 0]

testSparsifyEdgeCase :: Test
testSparsifyEdgeCase =
    TestCase $
        assertEqual
            "testSparsifyEdgeCase"
            (sparsify [Cell (0, 0) 1, Cell (0, 1) 0, Cell (0, 2) (-1), Cell (0, 3) 0])
            [Cell (0, 0) 1, Cell (0, 1) 0, Cell (0, 2) (-1), Cell (0, 3) 0]

testSparsifyComplex :: Test
testSparsifyComplex =
    TestCase $
        assertEqual
            "testSparsifySimple"
            ( sparsify
                [ Cell (0, 0) 1
                , Cell (0, 1) 0
                , Cell (0, 2) (-1)
                , Cell (0, 3) (-1)
                , Cell (0, 4) (-1)
                , Cell (0, 5) (-1)
                , Cell (0, 6) 0
                , Cell (0, 7) 1
                , Cell (0, 8) 0
                , Cell (0, 9) (-1)
                , Cell (0, 10) 0
                , Cell (0, 11) 0
                , Cell (0, 12) 0
                , Cell (0, 13) (-1)
                , Cell (0, 14) (-2)
                , Cell (0, 15) (-3)
                , Cell (0, 16) (-4)
                ]
            )
            [ Cell (0, 0) 1
            , Cell (0, 1) 0
            , Cell (0, 2) (-1)
            , Cell (0, 5) (-1)
            , Cell (0, 6) 0
            , Cell (0, 7) 1
            , Cell (0, 8) 0
            , Cell (0, 9) (-1)
            , Cell (0, 10) 0
            , Cell (0, 11) 0
            , Cell (0, 12) 0
            ]
