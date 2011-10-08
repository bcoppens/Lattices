module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Math.LinearAlgebra.GramSchmidt.Tests

main :: IO ()
main = defaultMain
    [
        testGroup "Math.LinearAlgebra.GramSchmidt.Tests" Math.LinearAlgebra.GramSchmidt.Tests.tests
    ]
