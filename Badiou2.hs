module Badiou2 where

import Control.Category
import Prelude hiding (id, (.))
import Numeric.Natural


data Badiou2Obj
    = R
    | N
    | T
    deriving (Show,Eq)

type R = Double
type N = Natural
data Terminal = ()

data Badiou2Morph
    = Real2Nat
    | Nat2Term
    deriving (Show, Eq)

data Badiou a b = Morph BadiouMorph

instance Category Badiou where
    id = Morph (Real2Nat Nat2Term Nat2Term)

