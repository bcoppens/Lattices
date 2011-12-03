module Math.Lattices.LLL.Tests (
    tests
) where

import           Test.Framework
import qualified Test.HUnit                     as H
import           Test.Framework.Providers.HUnit

import           Data.Ratio
import           Data.Array
import           Math.Lattices.LLL

equalsArray computed ok = H.assert $ elems computed == ok

simpleLLLTest1 = equalsArray computed ok
    where
        ok       = [ [1 % 1,2 % 1], [9 % 1,(-4) % 1] ]
        computed = lll $ [ [12, 2], [13, 4] ]

simpleLLLTest2  = equalsArray computed ok
    where
        ok       = [ [1 % 1,0 % 1,0 % 1], [0 % 1,2 % 1,0 % 1], [0 % 1,0 % 1,3 % 1] ]
        computed = lll $ [ [1, 0, 0], [4, 2, 15], [0, 0, 3] ]

tests :: [Test]
tests = concat
    [
      [testCase "Simple LLL test 1" simpleLLLTest1],
      [testCase "Simple LLL test 2" simpleLLLTest2]
    ]

