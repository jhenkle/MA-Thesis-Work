module Imperialism where

-- Objects 
data N = N -- Nature
data U = U -- Universal Kernel

-- Product of $U$ with itself ($U \times U$)
type UxU = (U,U)

-- Exponential Object ($U^U$)
type UtoU = U -> U 

-- Morphisms

-- Primitive Accumulation: $N \to U$
primAcc :: N -> U
primAcc _ = U

-- Idempotence: $U \cong (U \times U)$
idem :: U -> (U,U)
idem u = (u,u)

-- Autopoiesis: $U \cong U^U$
auto :: U -> UtoU
auto u = \_ -> u

-- Curry Correspondence: $(U \times U) \cong U^U$
curry :: UxU -> UtoU
curry (u1, u2) = \_ -> u1

-- Closed Cartesian Category Rules
class CCC cat where
    -- Terminal object
    terminal :: cat a ()

    -- Products
    pair :: cat a b -> cat a c -> cat a (b, c)
    fst :: cat (a, b) a
    snd :: cat (a, b) b

    -- Exponentials
    cur :: cat (a, b) c -> cat a (b -> c)
    uncur :: cat a (b -> c) -> cat (a, b) c
