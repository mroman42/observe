{-# LANGUAGE RebindableSyntax #-}

module NormBag where

import Prelude hiding ((>>=), (>>), return)
import FinMonad.NormalizedDistribution hiding ((>>=), (>>), return)
import qualified FinMonad.NormalizedDistribution as N
import FinMonad.Bag hiding ((>>=), (>>), return)
import qualified FinMonad.Bag as B
import BagSequencing

newtype NormBag a = NormBag (Normalized (Bag a))

unNormBag :: NormBag a -> Normalized (Bag a)
unNormBag (NormBag x) = x

instance (Eq a) => Eq (NormBag a) where
    (==) :: (Eq a) => NormBag a -> NormBag a -> Bool
    (==) (NormBag xs) (NormBag ys) = xs == ys

(>>=) :: (Eq a, Eq b) => NormBag a -> (a -> NormBag b) -> NormBag b
(>>=) (NormBag x) f = NormBag
    $ fJoin
    $ fMap (fMap fJoin)
    $ fMap blSeq 
    $ fMap (fMap (unNormBag . f)) x

(>>) :: (Eq a, Eq b) => NormBag a -> NormBag b -> NormBag b
(>>) d f = d >>= const f

return :: (Eq a) => a -> NormBag a
return = NormBag . fReturn . fReturn

distribution :: (Eq a) => [(a,Rational)] -> NormBag a
distribution l = NormBag $ fMap fReturn $ N.distribution l

bag :: (Eq a) => [a] -> NormBag a
bag = NormBag . fReturn . B.bag