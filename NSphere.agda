module NSphere where
  open import Cubical.Foundations
  open import Cubical.Path
  open import Cubical.Data.Nat
  open import Cubical.HITs.Suspension

  -- Base case: 0-sphere ($S^0$)
  |$S^0$| : Set
  |$S^0$| = Bool  -- $S^0$ is just two points: true and false

  -- Inductive definition of n-sphere ($S^n$)
  |$S^n$| : |$\mathbb{N}$| → Set
  |$S^n$| 0       = |$S^0$|                         
  -- Base case: 0-sphere
  |$S^0$| (suc n) = Suspension (|$S^n$| n)            
  -- Suspension of the (n-1)-sphere

  -- Example: Explicitly defining S¹ and S²
  S¹ : Set
  S¹ = |$S^n$| 1                   
  -- S¹ = Suspension of $S^0$

  S² : Set
  S² = |$S^n$| 2                                
  -- S² = Suspension of S¹

  -- Function to illustrate working with spheres
  -- A constant map from $S^n$ n to Unit
  constMap : |$\forall$| {n} → |$S^n$| n → Unit
  constMap _ = tt                          
  -- Every point maps to the unit element

  -- A dependent function on S¹
  exampleFuncOnS¹ : S¹ → Set
  exampleFuncOnS¹ north = |$\mathbb{N}$|                
  -- North pole maps to $\mathbb{N}$
  exampleFuncOnS¹ south = Bool              
  -- South pole maps to Bool
  exampleFuncOnS¹ (merid true i) = |$\mathbb{N}$|       
  -- Path: Meridians map to $\mathbb{N}$
  exampleFuncOnS¹ (merid false i) = Bool    
  -- Path: Meridians map to Bool