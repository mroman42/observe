{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE QualifiedDo #-}

module ExampleMontyNorm where

import Prelude hiding ((>>=), (>>), return, Left, Right)
import FinMonad.NormalizedDistribution
import FinMonad.Subdistribution qualified as S
import FinMonad.Subdistribution (Subdistribution(Subdistribution))



data Door = Left | Middle | Right deriving (Eq, Show)

-- Given car and choice, give the announcement
host :: Door -> Door -> Normalized Door
host Left   Left   = uniform [Middle, Right]
host Middle Middle = uniform [Left, Right]
host Right  Right  = uniform [Left, Middle] 
host Left Middle   = uniform [Right]
host Left Right    = uniform [Middle]
host Middle Left   = uniform [Right]
host Middle Right  = uniform [Left]
host Right Left    = uniform [Middle]
host Right Middle  = uniform [Left]


montyHall :: Normalized Door
montyHall = do
  (car, announcement) <- do
    (car, choice, announcement) <- do    
      car <- uniform [Left,Middle,Right]
      choice <- uniform [Left,Middle,Right]
      announcement <- host car choice
      return (car, choice, announcement)
    intervene (choice == Middle)
    return (car, announcement)
  intervene (announcement == Left)
  return car
--- >>> montyHall
-- Just <Distribution> [(Middle,1 % 3),(Right,2 % 3)]


montyHall2 :: Normalized Door
montyHall2 = do
  (car, announcement) <- do
      car <- uniform [Left,Middle,Right]
      choice <- return Middle
      announcement <- host car choice
      return (car, announcement)
  intervene (announcement == Left)
  return car
--- >>> montyHall2
-- Just <Distribution> [(Middle,1 % 3),(Right,2 % 3)]
  


montyHall3 = do
  car <- uniform [Left,Middle,Right]
  choice <- return Middle
  announcement <- host car choice
  intervene (announcement == Left)
  return car
--- >>> montyHall3
-- Just <Distribution> [(Middle,1 % 2),(Right,1 % 2)]


montyHall4 = normalize $ S.do
  car <- S.uniform [Left,Middle,Right]
  choice <- S.return Middle
  announcement <- host car choice
  S.observe (announcement == Left)
  S.return car
    where
      host :: Door -> Door -> Subdistribution Door
      host Left   Left   = S.uniform [Middle, Right]
      host Middle Middle = S.uniform [Left, Right]
      host Right  Right  = S.uniform [Left, Middle] 
      host Left Middle   = S.uniform [Right]
      host Left Right    = S.uniform [Middle]
      host Middle Left   = S.uniform [Right]
      host Middle Right  = S.uniform [Left]
      host Right Left    = S.uniform [Middle]
      host Right Middle  = S.uniform [Left]
      
--- >>> montyHall4
-- Just <Distribution> [(Middle,1 % 3),(Right,2 % 3)]
