module Badiou where

import Numeric.Natural

-- Definitions for R, N, and Terminal
type R = Double
type N = Natural
data Terminal = Terminal deriving Show

-- Functor Auth: Maps R to N
data Auth a = Auth (a -> N)

-- Functor Lack: Maps N to Terminal
data Lack a = Lack Terminal

-- Functor instance for Lack
instance Functor Lack where
    fmap _ _ = Lack Terminal

-- Composed Functor: Maps R to Terminal through Auth and Lack
data Composed a = Composed (a -> Terminal)

-- Implementation of composeAuthLack
composeAuthLack :: Auth R -> Lack N -> Composed R
composeAuthLack (Auth authFunc) _ = Composed (\x -> let _ = authFunc x in Terminal)

-- Example Instances
authRToN :: Auth R
authRToN = Auth (\x -> if x >= 0 && fromIntegral (truncate x :: Integer) == x
                        then fromIntegral (truncate x :: Integer)
                        else 0)

lackNToT :: Lack N
lackNToT = Lack Terminal

-- Composed Functor
composedFunctor :: Composed R
composedFunctor = composeAuthLack authRToN lackNToT