{-# LANGUAGE RebindableSyntax #-}

module ExampleSleeping where

import Subdistribution
import Prelude hiding ((>>=), (>>), return, Left, Right)
import Data.MultiSet qualified as MSet
import Subdistribution (uniform)


multiset :: (Ord a) => [a] -> MSet.MultiSet a
multiset = MSet.fromList

isIn :: (Ord a) => a -> MSet.MultiSet a -> Bool
isIn = MSet.member


data Coin = Heads | Tails  deriving (Eq, Show, Ord)
data Awakening = Awakening deriving (Eq, Show, Ord)


sleepingBeauty :: Distribution Coin
sleepingBeauty = do
  coin <- uniform [Heads, Tails]
  observe (Awakening `isIn` (case coin of
    Heads -> multiset [Awakening, Awakening]
    Tails -> multiset [Awakening]))
  return coin