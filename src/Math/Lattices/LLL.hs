-- | Implements a *very* basic LLL (Lenstra-Lenstra-Lovász) lattice reduction algorithm.
module Math.Lattices.LLL (
    lll,
    lllDelta
) where

import           Data.Ratio
import           Math.Algebra.LinearAlgebra
import           Math.LinearAlgebra.GramSchmidt

-- The $B_i$ set is called 'bb in this file, because of course we cannot call it 'B in Haskell.

-- | Just an easy way to write $||v||^2$
norm2 v = v <*> v

-- | Closest 'Integral to the given n, rounding down (TODO: up?). $\lfloor n\rceil$
-- rnd = round

-- | Return an LLL reduced basis. This calls 'lllDelta with a default parameter $\delta = 3/4$
lll basis = lllDelta basis $ 3%4

-- | Return an LLL reduced basis, with reduction parameter $\delta$. This is the conventional flavor of the algorithm using Gram-Schmidt, no fancy speedups yet
lllDelta basis delta = lll' basis delta gsB mu bi 1
    where
        (gsB, mu) = gramSchmidtOrthogonalization basis
        bi        = map norm2 gsB

-- | Perform a size reduction. Returns the new $b_k$, the new $\mu_k$. TODO: no new B_k, which is different from Steven Galbraith's explanation?
sizeReduction b' k b mu = sizeReduction' b' k b_js b_k mu_k $ k-1
    where
        b_js = reverse $ take k b
        mu_k = mu !! (k-1)
        b_k  = b  !! k

-- | Size reduction of the basis. Needs speeding up in lots of ways!. 'b_j:bs' is ordered to _descending js: $j = k - 1 \ldots 0$!.
--   Returns a tuple of the new $b_k$ and the new $\mu_k$.
sizeReduction' _  _ []       b_k mu_k (-1)  = (b_k, mu_k)
sizeReduction' b' k (b_j:bs) b_k mu_k j     = sizeReduction' b' k bs b_k' mu_k' $ j - 1
    where
        q_j    = toRational $ round $ mu_k !! j        -- TODO, sigh
        b_k'   = b_k <-> (q_j *> b_j)
        mu_k'  = take k $ map (slowMu b_k') b'

-- | Temporary function, we shouldn't recompute this every time!
slowMu b_i b'_j = (b_i <.> b'_j) / (b'_j <.> b'_j)

-- | Returns whether the Lovász Condition holds: $B_k \geq \delta - \mu^2_{k,k-1}B_{k-1}$
lovaszCondition bb k delta mu = bb !! k >= (delta - m*m)*(bb !! (k-1))
    where
        m = (mu !! k) !! (k-1)

lll' = undefined
{-
lll' basis delta gsB mu bi k | k <= n    = lllLoop
                             | otherwise = basis
                             where
                                n = undefined

-- | The main loop of the LLL algorithm. We reduce basis 'b with $\delta$ 'delta, with a Gram-Schmidt basis $b^*$ as 'b and the $\mu_{i,j}$ coefficients in 'mu.
--   The current iteration of the loop is 'k out of a maximum of 'n
lllLoop b delta b mu k n | lovaszCondition bb k delta mu = lllLoop ... $ min 1 $ k - 1
                             | otherwise                     = lllLoop basis delta gsB mu bi $ k + 1
    then
        
    where
        (bi', mu') = sizeReduction
-}
