-- | Implements a *very* basic LLL (Lenstra-Lenstra-Lovász) lattice reduction algorithm.
module Math.Lattices.LLL (
    lll,
    lllDelta
) where

import           Data.Array
import           Data.Ratio
import           Math.Algebra.LinearAlgebra     hiding ((!))
import           Math.LinearAlgebra.GramSchmidt

type Basis = Array Int [Rational]
type GSO   = Array (Int, Int) Rational

-- The $B_i$ set is called 'bb in this file, because of course we cannot call it 'B in Haskell.

-- TODO ### use STUArray instead of lists, AAAARG silly Haskell

-- | Just an easy way to write $||v||^2$
norm2 v = v <.> v

-- | Closest 'Integral to the given n, rounding down (TODO: up?). $\lfloor n\rceil$
-- rnd = round

-- | Return an LLL reduced basis. This calls 'lllDelta with a default parameter $\delta = 3/4$
lll basis = lllDelta basis $ 3%4

-- | Return an LLL reduced basis, with reduction parameter $\delta$. This is the conventional flavor of the algorithm using Gram-Schmidt, no fancy speedups yet
lllDelta basis delta = lllLoop b' delta bb' mu_arr 1 n
    where
        n       = length basis - 1
        (b, mu) = gramSchmidtOrthogonalization basis
        bb      = map norm2 b

        b'      = listArray (0, n) basis
        bb'     = listArray (0, n) bb

        -- TODO: reuse mu from GSO!!!
        mu_arr  = array ( ((0, 0), (n, n) ) ) [ ( (i,j), m ) | i     <- [0..n],
                                                               j     <- [0..n],
                                                               let m = (basis !! i <.> (b !! j)) / (norm2 $ b !! j) ]
{-
        mu_arr  = array ( (0, 0), (n, n) ) [ ((i,j), m) | i     <- [0..n],
                                                          j     <- [0..n],
                                                          let m = if ( i == j) then (toRational 1) else (toRational 0) ]
        mu_arr' = mu_arr' // [ ( (i, j), m ) | i     <- [0..n-1],
                                               j     <- [0..n-1],
                                               let m =  (mu !! i) !! j ] -- TODO
-}

-- | Perform a size reduction. Returns the new $b_k$, the new $\mu_k$.
sizeReduction :: Int -> Basis -> GSO -> (Basis, GSO)
sizeReduction k b mu = sizeReduction' indices k b mu
    where
        indices = reverse $ [0..k-1]

sizeReduction' (l:ls) k b mu = sizeReduction' ls k b' mu'
    where
        (b', mu') = sizeReduction'' k l b mu
sizeReduction' []     _ b mu = (b, mu)

-- | Size reduction of the basis for a single index (k, l). Returns a tuple of the new $b_k$ and the new $\mu_k$.
sizeReduction'' k l b mu = (b', mu'')
    where
        r    = toRational $ round $ mu ! (k, l)

        b_k  = b ! k
        b_l  = b ! l
        b_k' = b_k <-> (r *> b_l)
        b'   = b // [ (k, b_k') ]

        mu'  = mu  // [ update | j      <- [0..l-1],
                                 update <- [ ( (k, j), mu ! (k,j) - (r * mu ! (l,j)) ) ] ]
        mu'' = mu' // [ ( (k, l), mu' ! (k, l) - r) ]

-- | Returns whether the Lovász Condition holds: $B_k \geq \delta - \mu^2_{k,k-1}B_{k-1}$
lovaszCondition :: Array Int Rational -> Int -> Rational -> GSO -> Bool
lovaszCondition bb k delta mu = (bb ! k) >= (delta - m^2)*(bb ! (k-1))
    where
        m = mu ! (k, k-1)

-- | Swaps $b_k$ and $b_{k-1}$, returns a triple: (new $b$, new $B$, new $\mu$)
swapBaseVectors b bb mu_ k n = (b', bb', mu'')
    where
        b'    = b // [ (k - 1, b ! k), (k, b ! (k-1)) ]

        m     = mu_ ! (k, k-1)
        bb_k1 = bb  ! (k-1)
        bb_k  = bb  ! k

        btmp  = bb_k + m^2*bb_k1

        bb'   = bb  // [ (k, bb_k1*bb_k/btmp), (k-1, btmp) ]

        mu    = mu_ // [ ( (k, k-1), m*bb_k1/btmp ) ]

        mu'   = mu  // [ update | j      <- [0..k-2],
                                  update <- [ ( (k-1, j), mu!(k,j) ), ( (k, j), mu!(k-1, j)) ] ]
        mu''  = mu' // [ update | i      <- [k+1..n],
                                  update <- [ ( (i, k-1), update_i_k1 i), ( (i, k), update_i_k i) ] ]
                where
                    update_i_k1 i = (mu' ! (k, k-1)) * (mu' ! (i, k-1)) + (mu' ! (i, k)) - m*(mu' ! (i, k)) * (mu' ! (k, k-1))
                    update_i_k  i = (mu' ! (i, k-1)) - m * (mu' ! (i, k))

-- | The main loop of the LLL algorithm. We reduce basis 'b with $\delta$ 'delta, with a Gram-Schmidt basis $b^*$ as 'b and the $\mu_{i,j}$ coefficients in 'mu.
--   The current iteration of the loop is 'k out of a maximum of 'n
lllLoop :: Basis -> Rational -> Array Int Rational -> GSO -> Int -> Int -> Basis
lllLoop b delta bb mu k n | k > n     = b
                          | isLovasz  = lllLoop b'  delta bb  mu'  (k+1) n
                          | otherwise = lllLoop b'' delta bb' mu'' nextk n
    where
        (b', mu')        = sizeReduction k b mu
        isLovasz         = lovaszCondition bb k delta mu'

        (b'', bb', mu'') = swapBaseVectors b' bb mu' k n
        nextk            = max 1 $ k - 1
