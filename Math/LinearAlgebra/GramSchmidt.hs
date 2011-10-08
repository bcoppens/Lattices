-- | Compute a Gram-Schmidt orthogonal basis
module Math.LinearAlgebra.GramSchmidt (
    gramSchmidtBasis,
    gramSchmidtOrthogonalization
) where

import           Math.Algebra.LinearAlgebra

-- | Given a basis, return the Gram-Schmidt orhthogonal basis
gramSchmidtBasis = fst . gramSchmidtOrthogonalization

-- | Given a basis, return the Gram-Schmidt orthogonalization, which is a tuple with the Gram-Schmidt orthogonal basis first, and the
--   $\mu_{i,j} = \langle b_i, b^*_j \rangle / \langle b^*_j, b^*_j \rangle$ triangular matrix second, for $1 \leq j < i < n$.
gramSchmidtOrthogonalization basis@(b0:bs) = (undefined, undefined)
    where n = length basis

gs' = undefined

-- | Compute a (partial) row of the $\mu_{i,j}$ matrix. This is based on the previously orthogonalized vectors $b^*_j$, and the current vector $b_i$.
--   This assumes that 'b_i is of length 'i
mu_i b' b_i = flip map b' $ \b'_j -> (b_i <.> b'_j) / (b'_j <.> b'_j)
