{-# OPTIONS --cubical #-}
module ContractibleEmptyUnit where

open import Agda.Primitive
open import Cubical.Core.Everything
open import Data.Empty  -- Import for the empty type
open import Data.Unit   -- Import for the unit type
open import Data.Bool   -- Import for Bool
open import Data.Nat    -- Import for $\mathbb{N}$ (natural numbers)

-- Define S as the empty type ($\varnothing$)
S : Set
S = |$\bot$|  -- $\bot$ is the empty type in Agda

-- Define O as the unit type ({$\varnothing$})
O : Set
O = |$\top$|  -- $\top$ is the unit type in Agda

-- Show that S () is contractible
-- The empty type is trivially contractible since it has no elements.
contractible_S : Contr S
contractible_S = contr (λ x → x , λ _ → |$\bot$|-elim x)

-- Show that O ({$\varnothing$}) is contractible
-- The unit type is contractible because there is only one element (tt).
contractible_O : Contr O
contractible_O = contr tt λ _ → refl

-- Define two types alpha (α) and beta (β)
α : Set
α = |$\mathbb{N}$|  -- alpha is the type of natural numbers

β : Set
β = Bool  -- beta is the type of booleans

-- Define a function f : S → α (from $\varnothing$ to $\mathbb{N}$)
f : S → α
f x = |$\bot$|-elim x  -- No function can map from an empty type

-- Define a function g : O → β (from {$\varnothing$} to Bool)
g : O → β
g tt = true  -- Since O is unit type, we just map the single element to true

-- Define the type of morphisms (Hom) in a groupoid as paths
Hom : (X Y : Set) → Set
Hom X Y = X |$\equiv$| Y  -- Hom-sets are path types in HoTT

-- Define identity morphism (id) for any type X
id : {X : Set} → Hom X X
id = refl  -- The identity morphism is reflexivity

-- Define composition of morphisms in the groupoid
comp : {X Y Z : Set} → Hom X Y → Hom Y Z → Hom X Z
comp p q = p |$\bullet$| q  -- Composition is path concatenation ($\bullet$)

-- Define inverse of morphisms
inv : {X Y : Set} → Hom X Y → Hom Y X
inv p = sym p  -- Inverse is just path symmetry

-- Groupoid laws

-- Associativity of composition
comp_assoc : {X Y Z W : Set} (p : Hom X Y) (q : Hom Y Z) (r : Hom Z W) →
  comp p (comp q r) |$\equiv$| comp (comp p q) r
comp_assoc p q r = refl  -- Composition is associative

-- Left identity law: composing with the identity on the left
left_id : {X Y : Set} (p : Hom X Y) → comp id p |$\equiv$| p
left_id p = refl  -- Composing with identity on the left does nothing

-- Right identity law: composing with the identity on the right
right_id : {X Y : Set} (p : Hom X Y) → comp p id |$\equiv$| p
right_id p = refl  -- Composing with identity on the right does nothing

-- Left inverse law: composing a morphism with its inverse on the left gives the identity
inv_left : {X Y : Set} (p : Hom X Y) → comp (inv p) p |$\equiv$| id
inv_left p = refl  -- Composing a morphism with its inverse on the left gives identity

-- Right inverse law: composing a morphism with its inverse on the right gives the identity
inv_right : {X Y : Set} (p : Hom X Y) → comp p (inv p) |$\equiv$| id
inv_right p = refl  -- Composing a morphism with its inverse on the right gives identity

-- Define the Groupoid structure as a record in Agda
record Groupoid : Set where
  field
    Obj : Set  -- The objects in the groupoid
    Hom : Obj → Obj → Set  -- The morphisms (Hom-sets)
    id : (X : Obj) → Hom X X  -- Identity morphisms
    comp : {X Y Z : Obj} → Hom X Y → Hom Y Z → Hom X Z  -- Composition of morphisms
    inv : {X Y : Obj} → Hom X Y → Hom Y X  -- Inverse of morphisms

    -- Groupoid laws
    assoc : {X Y Z W : Obj} (p : Hom X Y) (q : Hom Y Z) (r : Hom Z W) →
      comp p (comp q r) |$\equiv$| comp (comp p q) r  -- Associativity
    id_left : {X Y : Obj} (p : Hom X Y) → comp (id X) p |$\equiv$| p  -- Left identity
    id_right : {X Y : Obj} (p : Hom X Y) → comp p (id Y) |$\equiv$| p  -- Right identity
    inv_left : {X Y : Obj} (p : Hom X Y) → comp (inv p) p |$\equiv$| id X  -- Left inverse
    inv_right : {X Y : Obj} (p : Hom X Y) → comp p (inv p) |$\equiv$| id Y  -- Right inverse

-- Example groupoid G with objects α and β
G : Groupoid
G = record {
  Obj = α |$\uplus$| β  -- Objects are the disjoint union of $\mathbb{N}$ and Bool
  ; Hom = λ X Y → X |$\equiv$| Y  -- Hom-sets are path types
  ; id = λ X → refl  -- Identity morphism is reflexivity
  ; comp = λ {X Y Z} p q → p |$\bullet$| q  -- Composition is path concatenation
  ; inv = λ {X Y} p → sym p  -- Inverse is path symmetry
  ; assoc = λ _ _ _ _ _ _ _ → refl  -- Associativity holds trivially
  ; id_left = λ _ _ _ → refl  -- Left identity holds trivially
  ; id_right = λ _ _ _ → refl  -- Right identity holds trivially
  ; inv_left = λ _ _ _ → refl  -- Left inverse holds trivially
  ; inv_right = λ _ _ _ → refl  -- Right inverse holds trivially
}