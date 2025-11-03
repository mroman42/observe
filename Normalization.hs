{-# LANGUAGE RebindableSyntax #-}

module Normalization where


import Data.Maybe
import Subdistribution
import Prelude hiding ((>>=), (>>), return)

ifThenElse True  x y = x
ifThenElse False x y = y


norm :: (Eq a) => Distribution a -> Maybe (Distribution a)
norm d = ifThenElse (validity d == 0) Nothing (Just (normalize d))

subd :: (Eq a) => Maybe (Distribution a) -> Distribution a
subd = fromMaybe (Distribution [])

normalizationBox :: (Eq a) => Distribution a -> Distribution a
normalizationBox = subd . norm

