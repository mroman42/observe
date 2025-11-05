{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}


module Distribution 
    ( Distribution (..)
    , distribution
    , uniform
    , toList
    , fromList
    , weightOf
    , map
    , (>>=)
    , (>>)
    , return
    , bind
    , next
    , pure
    ) where

import Prelude hiding ((>>=), (>>), return, pure, map)
import Prelude qualified as P
import Measure (Measure (..))
import Measure qualified as M
import FinitaryMonad

data Distribution a where
    Distribution :: (Eq a) => Measure a -> Distribution a

distribution :: (Eq a) => [(a, Rational)] -> Distribution a
distribution = fromList

mkDistribution :: (Eq a) => Measure a -> Distribution a
mkDistribution m = case M.validity m of
    1 -> Distribution m
    _ -> error "Non-full support for a distribution."


toList (Distribution m) = M.toList m
fromList l = mkDistribution (M.fromList l)
weightOf x (Distribution m) = M.weightOf x m
simplify (Distribution m) = Distribution $ M.simplify m

distToMeas :: Distribution a -> Measure a
distToMeas (Distribution a) = a

uniform :: (Eq a) => [a] -> Distribution a
uniform l = Distribution $ M.uniform l

instance (Eq a) => Eq (Distribution a) where
    (==) :: Distribution a -> Distribution a -> Bool
    (==) (Distribution a) (Distribution b) = (a == b)


instance FinitaryMonad Distribution where
    (>>=) :: (Eq a, Eq b) => Distribution a -> (a -> Distribution b) -> Distribution b
    (>>=) (Distribution d) f = Distribution $ M.measBind d (distToMeas . f)

    (>>) :: (Eq a, Eq b) => Distribution a -> Distribution b -> Distribution b
    (>>) d f = d >>= const f

    return :: (Eq a) => a -> Distribution a
    return x = uniform [x]

    fMap :: (Eq a, Eq b) => (a -> b) -> Distribution a -> Distribution b
    fMap = map 

bind :: (Eq a, Eq b) => Distribution a -> (a -> Distribution b) -> Distribution b
bind = (>>=)

next :: (Eq a, Eq b) => Distribution a -> Distribution b -> Distribution b
next = (>>)

pure :: (Eq a) => a -> Distribution a
pure = return

map :: (Eq a, Eq b) => (a -> b) -> Distribution a -> Distribution b
map f (Distribution (Measure d)) = Distribution $ Measure $ P.map (\(x,v) -> (f x, v)) d

instance (Eq a, Show a) => Show (Distribution a) where
  show :: (Eq a) => Distribution a -> String
  show (Distribution (Measure d)) = "<Distribution>\n" ++ show d