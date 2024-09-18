{-# LANGUAGE RebindableSyntax #-}

module ExampleSleepingTwo where

import Subdistribution
import Prelude hiding ((>>=), (>>), return, Left, Right)
import Data.MultiSet qualified as MSet
import Subdistribution (uniform)
import GHC.Builtin.Names (toAnnotationWrapperIdKey)


multiset :: (Ord a) => [a] -> MSet.MultiSet a
multiset = MSet.fromList

isIn :: (Ord a) => a -> MSet.MultiSet a -> Bool
isIn = MSet.member



data Coin = Heads | Tails  deriving (Eq, Show, Ord)
data Day = Monday | Tuesday  deriving (Eq, Show, Ord)

sleepingBeauty :: Distribution Coin
sleepingBeauty = do
  coin <- uniform [Heads, Tails]
  awakening <- uniform [(Heads, Monday), (Heads, Tuesday), (Tails, Monday)]
  assert (coin == fst awakening)
  return coin

sleepingNico :: Distribution Coin
sleepingNico = do
  coin <- uniform [Heads, Tails]
  awakening <- case coin of
    Heads -> uniform [(Heads, Monday), (Heads, Tuesday)]
    Tails -> uniform [(Tails, Monday)]
  assert (coin == fst awakening)
  return coin

sleepingWhere :: Distribution Coin
sleepingWhere = do
  coin <- uniform [Heads, Tails]
  observe (Awakening `isIn` (case coin of
    Heads -> multiset [Awakening, Awakening]
    Tails -> multiset [Awakening]))
  return coin  

