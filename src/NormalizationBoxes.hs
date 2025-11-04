{-# LANGUAGE RebindableSyntax #-}

module NormalizationBoxes (normalizationBox) where

import Data.Maybe
import Subdistribution
import Prelude hiding ((>>=), (>>), return)

ifThenElse True  x y = x
ifThenElse False x y = y


norm :: (Eq a) => Subdistribution a -> Maybe (Subdistribution a)
norm d = ifThenElse (validity d == 0) Nothing (Just (rescale d))

subd :: (Eq a) => Maybe (Subdistribution a) -> Subdistribution a
subd = fromMaybe (Subdistribution [])

normalizationBox :: (Eq a) => Subdistribution a -> Subdistribution a
normalizationBox = subd . norm

