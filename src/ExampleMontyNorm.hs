{-# LANGUAGE RebindableSyntax #-}

module ExampleMontyNorm where

import NormalizedDistribution
import SubdistributionAux
import Subdistribution qualified as Sub --hiding ((>>=), (>>), return)
import Prelude hiding ((>>=), (>>), return, Left, Right)
import Data.Maybe


data Door = Left | Middle | Right deriving (Eq, Show)

unif l = Just $ Sub.uniform l

-- Given car and choice, give the announcement
hostD :: Door -> Door -> Sub.Distribution Door
hostD Left   Left   = Sub.uniform [Middle, Right]
hostD Middle Middle = Sub.uniform [Left, Right]
hostD Right  Right  = Sub.uniform [Left, Middle] 
hostD Left Middle   = Sub.uniform [Right]
hostD Left Right    = Sub.uniform [Middle]
hostD Middle Left   = Sub.uniform [Right]
hostD Middle Right  = Sub.uniform [Left]
hostD Right Left    = Sub.uniform [Middle]
hostD Right Middle  = Sub.uniform [Left]

host :: Door -> Door -> Maybe (Sub.Distribution Door)
host d1 d2 = Just $ hostD d1 d2

-- This is associating to the left.
montyHall :: Maybe (Sub.Distribution Door)
montyHall = do
  (car, announcement) <- do
    (car, choice, announcement) <- do    
      car <- unif [Left,Middle,Right]
      choice <- unif [Left,Middle,Right]
      announcement <- host car choice
      return (car, choice, announcement)
    force (choice == Middle)
    return (car, announcement)
  force (announcement == Left)
  return car

-- >>> montyHall2
-- Just Validity: 1 % 1
-- Posterior: [(Middle,1 % 3),(Right,2 % 3)]
montyHall2 :: Maybe (Sub.Distribution Door)
montyHall2 = do
  (car, announcement) <- do
      car <- unif [Left,Middle,Right]
      choice <- return Middle
      announcement <- host car choice
      return (car, announcement)
  force (announcement == Left)
  return car

-- >>> montyHall3
-- Just Validity: 1 % 3
-- Posterior: [(Left,1 % 1)]
montyHall3 = do
  car <- unif [Left,Middle,Right]
  choice <- return Left
  announcement <- host car choice
  force (announcement == Middle)
  return car

--- ???

--- >>> (Just (Sub.fromList [((Left,Right),1 / 3),((Middle,Left),1 / 6),((Middle,Right),1 / 6),((Right,Left),1 / 3)])) >>= \(a,c) -> (intervene (a == Left))
-- Just Validity: 1 % 1
-- Posterior: [((),1 % 1)]
