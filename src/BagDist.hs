{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}


module BagDist where

import Prelude hiding ((>>=), (>>), return, pure, map)
import Distribution
import Bag
import FinitaryMonad

-- DBX is a commutative monoid.
multiply :: (Eq x) => 
  Distribution (Bag x) -> Distribution (Bag x) -> Distribution (Bag x)
multiply u v = do
  x <- u
  y <- v
  return (bagAdd x y)
  

diracEmptyBag :: (Eq a) => Distribution (Bag a)
diracEmptyBag = distribution [(bag [],1)]



diracBag :: (Eq a) => a -> Distribution (Bag a)
diracBag x = distribution [((bag [(x,1)]), 1)]



reduce :: (Eq a) => (a -> a -> a) -> a -> Bag a -> a
reduce m u (Bag []) = u
reduce m u (Bag ((x,0):l)) = reduce m u (bag l)
reduce m u (Bag ((x,n):l)) = m x (reduce m u (bag ((x,n-1):l)))

-- Something is wrong here.
distribute :: (Eq a) => Bag (Distribution a) -> Distribution (Bag a)
distribute = --fMap Bag (distList b)
  reduce multiply diracEmptyBag . (fMap (fMap fReturn))

--distWeightList :: (Eq a) => [(Distribution a, Int)] -> Distribution [a]


distList :: (Eq a) => [Distribution a] -> Distribution [a]
distList [] = return []
distList (d:l) = do
  x <- d
  xs <- distList l
  return (x:xs)


--- D-unitality
--- BX -> BDX -> DBX
--- BX -> DBX
dunitL :: (Eq a) => Bag a -> Distribution (Bag a) 
dunitL b = distribute $ fMap fReturn b
dunitR :: (Eq a) => Bag a -> Distribution (Bag a) 
dunitR b = fReturn b

--- >>> dunitL ex1 == dunitR ex1
ex1 :: Bag Char
ex1 = bag [('x',2),('y',3)]


--- D-multiplicativity
-- BDDX -> DBDX -> DDBX -> DBX
dmultL :: (Eq a) => Bag (Distribution (Distribution a)) -> Distribution (Bag a) 
dmultL x = fJoin $ fMap distribute $ distribute x

dmultR :: (Eq a) => Bag (Distribution (Distribution a)) -> Distribution (Bag a) 
dmultR x = distribute $ fMap fJoin x

ex2 :: Bag (Distribution (Distribution Char))
ex2 = bag
  [ (distribution 
      [ (distribution [('x', 1/3), ('y', 2/3)], 1/2)
      , (distribution [('x', 1/2), ('y', 1/2)], 1/2)
      ] , 2)
  , (distribution 
      [ (distribution [('x', 1/5), ('y', 4/5)], 1/5)
      , (distribution [('x', 1/3), ('z', 2/3)], 4/5)
      ], 3) ]

--- B-unitality
-- DX -> BDX -> DBX
-- DX -> DBX
bunitL :: (Eq a) => Distribution a -> Distribution (Bag a)
bunitL x = distribute $ fReturn x


-- BBDX -> BDBX -> DBBX -> DBX
-- BBDX -> BDX -> DBX
bmultL :: (Eq a) => Bag (Bag (Distribution a)) -> Distribution (Bag a)
bmultL x = fMap fJoin $  distribute $ fMap distribute x

bmultR :: (Eq a) => Bag (Bag (Distribution a)) -> Distribution (Bag a) 
bmultR x = distribute $ fJoin x

ex3 :: Bag (Bag (Distribution Char))
ex3 = bag
  [ (bag [ ], 1)
  , (bag [ (distribution [('y', 1/3), ('z', 2/3)], 1) ], 2) ]


ex4 :: Bag (Bag (Distribution Char))
ex4 = bag
  [ (bag [ (distribution [('x', 1/2), ('y', 1/2)], 1) ], 1)
  , (bag [ (distribution [('y', 1/3), ('z', 1/3), ('x',1/3)], 1) 
         , (distribution [('z', 1/5), ('y', 2/5), ('x',2/5)], 1)  
         ], 2) 
  , (bag [ (distribution [('y', 1)], 1) ], 1)
  ]

ex5 :: Bag (Bag (Distribution Char))
ex5 = bag
  [ (bag [ (distribution [('y', 2/3), ('z', 1/3)], 1) 
         , (distribution [('y', 1/3), ('z', 2/3)], 1) 
         ], 2) ]

ex6 :: Bag (Bag (Distribution Char))
ex6 = bag
  [ (bag [ (distribution [('x', 1)], 1) ], 1)
  , (bag [ (distribution [('y', 1)], 1) 
         , (distribution [('x', 1/2), ('y', 1/2)], 1)  
         ], 2) 
  ]



