{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE QualifiedDo #-}

module Multitesting where

import Prelude hiding ((>>=), (>>), return, Left, Right)
import MDB


data Result = Pos | Neg deriving (Eq, Show, Ord)
data Individual = Ill | Healthy deriving (Eq, Show)

testing :: Individual -> NormBag Result
testing Ill = fromList [(Pos, 9/10), (Neg, 1 / 10)]
testing Healthy = fromList [(Pos, 2/5), (Neg, 3/5)]
