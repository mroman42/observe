{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}


module BagDist where

import Prelude hiding ((>>=), (>>), return, pure, map)
import Distribution
import Bag
import FinitaryMonad

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
reduce m u (Bag ((x,0):l)) = reduce m u (Bag l)
reduce m u (Bag ((x,n):l)) = m x (reduce m u (Bag ((x,n-1):l)))

distribute :: (Eq a) => Bag (Distribution a) -> Distribution (Bag a)
distribute = reduce multiply diracEmptyBag . (fMap (fMap fReturn))