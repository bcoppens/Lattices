module Math.LinearAlgebra.GramSchmidt.Tests (
    tests
) where

import Test.Framework
import qualified Test.HUnit as H
import Test.Framework.Providers.HUnit

import Data.Ratio
import Math.LinearAlgebra.GramSchmidt

simpleTest = H.assert $ computed == correct
    where
        computed = gramSchmidtOrthogonalization $ map (map toRational) [ [1, 1, 0], [1, 0, 1], [0, 1, 1] ]
        correct  = ([[1 % 1,1 % 1,0 % 1],[1 % 2,(-1) % 2,1 % 1],[(-2) % 3,2 % 3,2 % 3]],[[1 % 2],[1 % 2,1 % 3]])

tests :: [Test]
tests = concat
    [
      [testCase "Simple G-S test on Rationals" simpleTest]
    ]

