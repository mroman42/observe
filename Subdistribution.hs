{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Subdistribution where

import Prelude hiding ((>>=), return)
import Data.Ord
import Data.Maybe
import Data.List ( maximumBy )
import GHC.Generics (Generic)
import SubdistributionAux

data Distribution a where
  Distribution :: (Eq a) => [(a, Rational)] -> Distribution a

unDistribution :: Distribution a -> [(a, Rational)]
unDistribution (Distribution d) = d

validity :: Distribution a -> Rational
validity (Distribution l) = totalWeight l

instance (Eq a) => Eq (Distribution a) where
  (==) :: Distribution a -> Distribution a -> Bool
  (==) (Distribution u) (Distribution v) = 
    isJust $ checkMaybe (reweight u) (reweight v)

(>>=) :: (Eq a, Eq b) => Distribution a -> (a -> Distribution b) -> Distribution b
(>>=) (Distribution d) f = Distribution $ distBind d (unDistribution . f)

dmap :: (Eq a, Eq b) => (a -> b) -> Distribution a -> Distribution b
dmap f d = d >>= (return . f)

(>>) :: (Eq a, Eq b) => 
  Distribution a -> Distribution b -> Distribution b
(>>) d f = d >>= const f

return :: (Eq a) => a -> Distribution a
return x = Distribution [(x,1)]


observe :: Bool -> Distribution ()
observe True = return ()
observe False = absurd

assert :: Bool -> Distribution ()
assert = observe

absurd :: (Eq a) => Distribution a
absurd = Distribution []

fromList, distribution :: (Eq a) => [(a,Rational)] -> Distribution a
fromList = Distribution . condense
distribution = fromList

toList :: (Eq a) => Distribution a -> [(a,Rational)]
toList = unDistribution

instance (Eq a, Show a) => Show (Distribution a) where
  show :: Eq a => Distribution a -> String
  show = showDistribution
  
showDistribution :: (Eq a, Show a) => Distribution a -> String
showDistribution d =
  "Validity: " ++ show (validity d) ++ "\n" ++
  "Posterior: " ++ show (unDistribution (normalize d)) ++ "\n"

normalize :: Distribution a -> Distribution a
normalize (Distribution x) = Distribution (distNormalize x)

uniform :: (Eq a) => [a] -> Distribution a
uniform l = fromList (map (, 1 / toRational (length l)) l)


