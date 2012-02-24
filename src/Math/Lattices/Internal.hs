-- | Some internal convenience functions for use in Math.Lattices
--
module Math.Lattices.Internal (
        norm2,
        rnd
) where

import           Data.Ratio
import           Math.Algebra.LinearAlgebra     hiding ((!))


-- | Just an easy way to write $||v||^2$
norm2 v = v <.> v

-- | Closest 'Integral to the given n, rounding up. $\lfloor n\rceil$
rnd x = floor $ x + 1%2
