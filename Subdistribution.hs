-- A trial implementation of a probabilistic programming 
-- setup with the primitives of a Partial Markov Category.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}


module Subdistribution where

import Prelude hiding ((>>=), return)
import Data.Finitary
import Data.Ord
import Data.Maybe
import Data.List ( maximumBy )
import GHC.Generics (Generic)
import SubdistributionAux

-- Finitary subdistribution monad, implemented with lists.
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


-- Rebindable do notation.
(>>=) :: (Eq a, Eq b) => Distribution a -> (a -> Distribution b) -> Distribution b
(>>=) (Distribution d) f = Distribution $ distBind d (unDistribution . f)

(>>) :: (Eq a, Eq b) => 
  Distribution a -> Distribution b -> Distribution b
(>>) d f = d >>= const f

-- Distribution combinators.
return :: (Eq a) => a -> Distribution a
return x = Distribution [(x,1)]


observe :: Bool -> Distribution ()
observe True = return ()
observe False = absurd

assert :: Bool -> Distribution ()
assert = observe



absurd :: (Finitary a) => Distribution a
absurd = Distribution []


fromList :: (Eq a) => [(a,Rational)] -> Distribution a
fromList = Distribution . condense

distribution :: (Eq a) => [(a,Rational)] -> Distribution a
distribution = fromList

toList :: (Eq a) => Distribution a -> [(a,Rational)]
toList = unDistribution

instance (Eq a, Show a) => Show (Distribution a) where
  show :: Eq a => Distribution a -> String
  show = showDistribution
  -- show = show . toList

showDistribution :: (Eq a, Show a) => Distribution a -> String
showDistribution d =
  "Validity: " ++ show (validity d) ++ "\n" ++
  "Posterior: " ++ show (unDistribution (normalize d)) ++ "\n"

normalize :: Distribution a -> Distribution a
normalize (Distribution x) = Distribution (distNormalize x)

uniform :: (Eq a) => [a] -> Distribution a
uniform l = fromList (map (, 1 / toRational (length l)) l)
