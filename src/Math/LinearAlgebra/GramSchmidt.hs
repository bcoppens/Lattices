-- | Compute a Gram-Schmidt orthogonal basis
module Math.LinearAlgebra.GramSchmidt (
    gramSchmidtBasis,
    gramSchmidtOrthogonalization
) where

import           Math.Algebra.LinearAlgebra

-- | Given a basis, return the Gram-Schmidt orhthogonal basis
gramSchmidtBasis a = fst $ gramSchmidtOrthogonalization a

-- | Given a basis, return the Gram-Schmidt orthogonalization, which is a tuple with the Gram-Schmidt orthogonal basis first, and the
--   $\mu_{i,j} = \langle b_i, b^*_j \rangle / \langle b^*_j, b^*_j \rangle$ triangular matrix second, for $1 \leq j < i < n$.
gramSchmidtOrthogonalization (b0:bs) = gs bs [b0] []

-- TODO get rid of the (++) used like this, to make it faster
-- | Perform actual Gram-Schmidt reduction
gs []       b' mu = (b', mu)
gs (b_i:bs) b' mu = gs bs b'' mu'
    where
        mu_i   = mu_row b' b_i
        mu'    = mu ++ [mu_i]
        tosum  = zipWith (*>) mu_i b'
        offset = foldl1 (<+>) tosum
        b'_i   = b_i <-> offset
        b''    = b' ++ [b'_i]

-- | Compute a (partial) row of the $\mu_{i,j}$ matrix. This is based on the previously orthogonalized vectors $b^*_j$, and the current vector $b_i$.
--   This assumes that 'b_i is of length 'i
mu_row b' b_i = flip map b' $ \b'_j -> (b_i <.> b'_j) / (b'_j <.> b'_j)
