-- | Approximation algorithm for the Closest Vector Problem. The default implementation
--   uses Babai's Nearest Plane Method. References:
--
--   * On Lovász' Lattice Reduction And The Nearest Lattice Point Problem, László Babai. Combinatorica 6 (1), 1-13 (1986).
--
--   * Mathematics of Public Key Cryptography, Steven Galbraith. Chapter 18 of draft 1.0
--
module Math.Lattices.CloseVector (
    closeVector
) where

import           Data.Array
import           Data.Ratio
import           Math.Algebra.LinearAlgebra     hiding ((!))
import           Math.Lattices.Internal
import           Math.Lattices.LLL
import           Math.LinearAlgebra.GramSchmidt


-- Babai's Algorithm for CVP

-- | Find a lattice vector in 'basis close to 'x'. 'basis' is assumed to be LLL-reduced
closeVector :: [[Rational]] -> [Ratio Integer] -> [Rational]
closeVector basis x = foldl1 (<+>) $ babaiNP (reverse $ [0..d]) basis' b' x
    where
        d      = length basis - 1
        b'     = listArray (0, d) $ gramSchmidtBasis basis
        basis' = listArray (0, d) basis

projectTo v b = (v <.> b) / (norm2 b)

vsum zero = foldl (<+>) zero

-- | Find a close vector to 'x using Babai's Nearest Plane Method. 'b is an LLL-reduced basis, 'b'' is its Gram-Schmidt basis d is the size of the (sub)space.
babaiNP []     _ _  _ = []
babaiNP (i:is) b b' w = y_i : recurse
    where
        l_i     = projectTo w $ b' ! i
        delta   = toRational $ rnd $ l_i
        y_i     = delta *> b ! i

        w_i1    = w <-> (l_i - delta) *> (b' ! i) <-> y_i

        recurse = babaiNP is b b' w_i1
