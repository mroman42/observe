{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}


module Distribution 
    ( Distribution (..)
    , uniform
    , (>>=)
    , (>>)
    , return
    , distribution
    ) where

import Prelude hiding ((>>=), (>>), return)
import Measure


data Distribution a where
    Distribution :: (Eq a) => Measure a -> Distribution a

distribution :: (Eq a) => [(a, Rational)] -> Distribution a
distribution = fromList

mkDistribution :: (Eq a) => Measure a -> Distribution a
mkDistribution m = case validity m of
    1 -> Distribution m
    _ -> error "Non-full support for a distribution."

instance Distributional Distribution where
    toList (Distribution m) = toList m
    fromList l = mkDistribution (fromList l)
    weightOf x (Distribution m) = weightOf x m
    simplify (Distribution m) = Distribution $ simplify m

distToMeas :: Distribution a -> Measure a
distToMeas (Distribution a) = a


(>>=) :: (Eq a, Eq b) => Distribution a -> (a -> Distribution b) -> Distribution b
(>>=) (Distribution d) f = Distribution $ measBind d (distToMeas . f)

(>>) :: (Eq a, Eq b) => Distribution a -> Distribution b -> Distribution b
(>>) d f = d >>= const f

return :: (Eq a) => a -> Distribution a
return x = uniform [x]

instance (Eq a, Show a) => Show (Distribution a) where
  show :: (Eq a) => Distribution a -> String
  show (Distribution (Measure d)) = "<Distribution>\n" ++ show d