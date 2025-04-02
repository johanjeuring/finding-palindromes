module ITLinear where

import Data.Algorithms.Palindromes.Combinators
    ( Complexity (..)
    , Output (..)
    , Variant (..)
    , createReadableCombinator
    )
import Test.HUnit (Test (..), (~:), (~?=))

testListITLinear =
    [ TestLabel "testITLinear1" testITLinear1
    , TestLabel "testITLinear2" testITLinear2
    ]

testITLinear1 =
    "testITLinear1"
        ~: createReadableCombinator
            VarWord
            OutWord
            ComLinear
            (0, Nothing)
            "abc def def abc"
        ~?= "abc def def abc"
testITLinear2 =
    "testITLinear2"
        ~: createReadableCombinator
            VarWord
            OutWord
            ComLinear
            (0, Nothing)
            "abc def def abc"
        ~?= "abc def def abc"
