module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Math.LinearAlgebra.GramSchmidt.Tests
import qualified Math.Lattices.LLL.Tests

main :: IO ()
main = defaultMain
    [
        testGroup "Math.LinearAlgebra.GramSchmidt.Tests" Math.LinearAlgebra.GramSchmidt.Tests.tests,
        testGroup "Math.Lattices.LLL.Tests" Math.Lattices.LLL.Tests.tests
    ]
