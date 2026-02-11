{-# LANGUAGE RebindableSyntax #-}

module ExampleSailor where

import Prelude hiding ((>>=), (>>), return, Left, Right)
import Data.MultiSet qualified as MSet
import FinMonad.Subdistribution


multiset :: (Ord a) => [a] -> MSet.MultiSet a
multiset = MSet.fromList

isIn :: (Ord a) => a -> MSet.MultiSet a -> Bool
isIn = MSet.member

data Port = Siracuse | Heraklion  deriving (Eq, Show, Ord)
data Coin = Heads | Tails  deriving (Eq, Show, Ord)

sailorChild :: Subdistribution Coin
sailorChild = do
  guide <- uniform [Siracuse, Heraklion]
  coin <- uniform [Heads, Tails]
  observe (Siracuse `isIn` (case (guide, coin) of
    (Siracuse, Tails) -> multiset [Siracuse, Heraklion]
    (Heraklion, Tails) -> multiset [Siracuse, Heraklion]
    (Siracuse, Heads) -> multiset [Siracuse]
    (Heraklion, Heads) -> multiset [Heraklion]))
  return coin

--- >>> sailorChild  
-- <Subdistribution>
-- Validity: 3 % 4
-- Posterior: <Distribution> [(Heads,1 % 3),(Tails,2 % 3)]
