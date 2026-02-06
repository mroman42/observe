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
    ) where

import Prelude hiding ((>>=), (>>), return, pure, map)
import Prelude qualified as P
import Measure (Measure (..))
import Measure qualified as M
import FinitaryMonad
import AuxiliarySemiring


data Distribution a where
    Distribution :: (Eq a) => Measure a -> Distribution a

fromList l = mkDistribution (M.fromList l)    

distribution :: (Eq a) => [(a, Rational)] -> Distribution a
distribution d = case totalWeight d of
    1 -> fromList $ condense d
    _ -> error "Non-full support for a distribution."

mkDistribution :: (Eq a) => Measure a -> Distribution a
mkDistribution m = case M.validity m of
    1 -> Distribution m
    _ -> error "Non-full support for a distribution."


toList (Distribution m) = M.toList m

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
    fBind :: (Eq a, Eq b) 
      => Distribution a -> (a -> Distribution b) -> Distribution b
    fBind (Distribution d) f = 
      Distribution $ M.measBind d (distToMeas . f)

    fNext :: (Eq a, Eq b) => Distribution a -> Distribution b -> Distribution b
    fNext d f = fBind d (const f)

    fReturn :: (Eq a) => a -> Distribution a
    fReturn x = uniform [x]

    fMap :: (Eq a, Eq b) => (a -> b) -> Distribution a -> Distribution b
    fMap = map 

(>>=) :: (Eq a, Eq b) => Distribution a -> (a -> Distribution b) -> Distribution b
(>>=) = fBind

(>>) :: (Eq a, Eq b) => Distribution a -> Distribution b -> Distribution b
(>>) = fNext

return :: (Eq a) => a -> Distribution a
return = fReturn

pure :: (Eq a) => a -> Distribution a
pure = return

map :: (Eq a, Eq b) => (a -> b) -> Distribution a -> Distribution b
map f (Distribution (Measure d)) = 
    distribution $ P.map (\(x,v) -> (f x, v)) d

instance (Eq a, Show a) => Show (Distribution a) where
  show :: (Eq a) => Distribution a -> String
  show (Distribution (Measure d)) = "<Distribution> " ++ show d