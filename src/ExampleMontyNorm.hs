{-# LANGUAGE RebindableSyntax #-}

module ExampleMontyNorm where

import NormalizedDistribution
import SubdistributionAux
import Subdistribution hiding ((>>=), (>>), return)
import Prelude hiding ((>>=), (>>), return, Left, Right)
import Data.Maybe


data Door = Left | Middle | Right deriving (Eq, Show)

unif l = Just $ uniform l

hostD :: Door -> Door -> Distribution Door
hostD Left   Left   = uniform [Middle, Right]
hostD Middle Middle = uniform [Left, Right]
hostD Right  Right  = uniform [Left, Middle] 
hostD Left Middle   = uniform [Right]
hostD Left Right    = uniform [Middle]
hostD Middle Left   = uniform [Right]
hostD Middle Right  = uniform [Left]
hostD Right Left    = uniform [Middle]
hostD Right Middle  = uniform [Left]

host :: Door -> Door -> Maybe (Distribution Door)
host d1 d2 = Just $ hostD d1 d2

montyHall :: Maybe (Distribution Door)
montyHall = do
  (car, choice, announcement) <- do
    car <- unif [Left,Middle,Right]
    choice <- unif [Left,Middle,Right]
    announcement <- host car choice
    return (car, choice, announcement)
  --force (choice == Middle)
  force (announcement == Left)
  return car
