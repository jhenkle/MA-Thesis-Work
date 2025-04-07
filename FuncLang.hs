module FuncLang where

import Data.Hashable (hash)

-- Subjective

-- Objects
data State
    = Initial         -- Represents $S$
    | Successor State  -- Represents $S^\prime$
    | Final           -- Represents $S^S$ (final state)
    deriving (Show, Eq)

-- Define the successor function
f :: State -> State
f Initial = Successor Initial  -- Transition from $S$ to $S^\prime$
f (Successor s) =
    case s of
        Final -> Final          -- If already at $S^S$, stay there
        _     -> Successor (f s) -- Otherwise, recurse deeper
f Final = Final  

-- Field of the Other

-- Objects

data U = U deriving (Show) -- Monoid of the Symbolic

-- Morphims

-- Define the morphisms as natural numbers
newtype Morphism = Morphism Int deriving (Show, Eq)

-- Identity morphism (corresponds to 0)
identityMorphism :: Morphism
identityMorphism = Morphism 0

-- Composition of morphisms
composeMorphisms :: Morphism -> Morphism -> Morphism
composeMorphisms (Morphism m) (Morphism n) = Morphism (m + n)

-- Action of a morphism on U (trivial in this case)
applyMorphism :: Morphism -> U -> U
applyMorphism _ U = U

-- Language Relation (Functor $j_0$)

-- On Objects
jOb :: State -> U
jOb _ = U

-- On Morphisms
jMorph :: (State -> State) -> [Morphism]
jMorph stateTransition =                -- stateTransition is a place-holder for the parameter 'f' 
    let initialState = Initial
        states = iterate f initialState  -- Generate infinite sequence of states
        indices = takeWhile (/= Final) states  -- Stop at Final state
        hashedIndices = map (\s -> Morphism (hash (show s) `mod` 100)) indices
    in hashedIndices

