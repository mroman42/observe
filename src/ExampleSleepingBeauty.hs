{-# LANGUAGE RebindableSyntax #-}

module ExampleSleepingBeauty where

import Subdistribution
import Prelude hiding ((>>=), (>>), return, Left, Right)

-- Some researchers are going to put you to sleep. During the two days that your
-- sleep will last, they will briefly wake you up either once or twice,
-- depending on the toss of a fair coin (Heads: once; Tails: twice). After each
-- waking, they will put you to back to sleep with a drug that makes you forget
-- that waking.
--
-- When you are first awakened, to what degree ought you believe that the
-- outcome of the coin toss is Heads?
--
-- -- Adam Elga, Self-locating Belief and the Sleeping Beauty Problem"


data Coin = Heads | Tails  deriving (Eq, Show, Ord)
data Day = Monday | Tuesday  deriving (Eq, Show, Ord)


sleeping1 :: Subdistribution Coin
sleeping1 = do
  coin <- uniform [Heads, Tails]
  awakening <- uniform [(Heads, Monday), (Heads, Tuesday), (Tails, Monday)]
  observe (coin == fst awakening)
  return coin

sleeping2 :: Subdistribution Coin
sleeping2 = do
  coin <- uniform [Heads, Tails]
  awakening <- case coin of
    Heads -> uniform [(Heads, Monday), (Heads, Tuesday)]
    Tails -> uniform [(Tails, Monday)]
  observe (coin == fst awakening)
  return coin
