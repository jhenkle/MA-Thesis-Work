module Lacan where

import Control.Category
import Prelude hiding (id, (.))

-- Objects in the category
data LacanObj
    = BigOther        -- A
    | PhiX String     -- Φx with a parameter (e.g., α, β, γ)
    | Petite_a        -- a
    deriving (Show, Eq)

-- Morphisms in the category
data LacanMorph
    = IdM LacanObj             -- Identity morphism for each object
    | Auth LacanObj LacanObj   -- auth: A → Φx
    | Lack LacanObj LacanObj   -- lack: Φx → a
    deriving (Show, Eq)

-- The Lacan category with objects and morphisms
data Lacan a b = Morph LacanMorph

instance Category Lacan where
    -- Identity morphism for each object
    id = Morph (IdM BigOther)  -- Default, but should be object-dependent

    -- Composition of morphisms
    Morph (IdM a) . f = f   -- Identity acts as a neutral element
    f . Morph (IdM a) = f

    Morph (Auth a b) . Morph (Auth _ c) 
        | b == c    = Morph (Auth a c)  -- Compose auth morphisms
        | otherwise = error "Invalid composition"

    Morph (Lack a b) . Morph (Auth _ c) 
        | b == c    = Morph (Lack a c)  -- Compose auth and lack morphisms
        | otherwise = error "Invalid composition"

    Morph (Lack a b) . Morph (Lack _ c)
        | b == c    = Morph (Lack a c)  -- Compose lack morphisms
        | otherwise = error "Invalid composition"

    _ . _ = error "Invalid composition"

-- Functor instance: Mapping Lacan to Haskell functions
instance Functor Lacan where
    fmap f (Morph (IdM x)) = Morph (IdM (f x))    -- Identity morphism mapping
    fmap f (Morph (Auth a b)) = Morph (Auth (f a) (f b))
    fmap f (Morph (Lack a b)) = Morph (Lack (f a) (f b))

-- auth. morphisms
authAlpha :: Lacan LacanObj LacanObj
authAlpha = Morph (Auth BigOther (PhiX "α"))

authBeta :: Lacan LacanObj LacanObj
authBeta = Morph (Auth BigOther (PhiX "β"))

authGamma :: Lacan LacanObj LacanObj
authGamma = Morph (Auth BigOther (PhiX "γ"))

-- lack morphisms
lackAlpha :: Lacan LacanObj LacanObj
lackAlpha = Morph (Lack (PhiX "α") Petite_a)

lackBeta :: Lacan LacanObj LacanObj
lackBeta = Morph (Lack (PhiX "β") Petite_a)

lackGamma :: Lacan LacanObj LacanObj
lackGamma = Morph (Lack (PhiX "γ") Petite_a)

-- Composition examples
authToLackAlpha :: Lacan LacanObj LacanObj
authToLackAlpha = lackAlpha . authAlpha -- BigOther → Φx_α → a

authToLackBeta :: Lacan LacanObj LacanObj
authToLackBeta = lackBeta . authBeta   -- BigOther → Φx_β → a

authToLackGamma :: Lacan LacanObj LacanObj
authToLackGamma = lackGamma . authGamma -- BigOther → Φx_γ → a
