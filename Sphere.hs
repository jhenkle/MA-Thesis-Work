module Sphere where

data Empty -- Empty Type

data Suspension a = North | South | Merid a -- Constructs Suspension

top :: Suspension () -- Pole Definition
top = North

merid :: a -> Suspension a -- Meridian Definition
merid x = Merid x

type Sphere0 = Suspension Empty -- Suspension on the Empty Type ($S^{-1}$ or Empty Set)

ex1 :: Suspension () -- Suspension on the Unit Type ($S^0$ or Point Set)
ex1 = North

ex2 :: Suspension ()
ex2 = Merid ()

ex3 :: Suspension Bool -- Suspension on the Boolean Type ($S^1$ or Loop)
ex3 = North

ex4 :: Suspension Bool
ex4 = Merid True

ex5 :: Suspension (Bool, Bool) -- Suspension on the Loop ($S^2$ or 3D Sphere)
ex5 = North

ex6 :: Suspension (Bool, Bool)
ex6 = Merid (True, False)

newtype Mu f = Roll { unRoll :: f (Mu f)} -- Recursive type definition
type InfSuspension = Mu Suspension -- Infinite Suspension definition

exInf :: Suspension (Mu Suspension) -- Suspension on Infinite recursive type ($S^{:lo\infty}$)
exInf = North

exInfty :: Suspension (Mu Suspension)
exInfty = Merid (Roll exInfty)