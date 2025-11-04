{-# LANGUAGE RebindableSyntax #-}

module ExampleMontyFall where

import Subdistribution
import Prelude hiding ((>>=), (>>), return, Left, Right)
import Data.MultiSet qualified as MSet


-- Suppose you’re on a game show, and you’re given the choice of three doors: Behind one
-- door is a car; behind the others, goats. You pick a door, say No. 1, and the host, who
-- knows what’s behind the doors, opens another door, say No. 3, which has a goat. He
-- then says to you, “Do you want to pick door No. 2?” Is it to your advantage to switch
-- your choice?

data Door = Left | Middle | Right deriving (Eq, Show)

host :: Door -> Door -> Subdistribution Door
host Left   Left   = uniform [Middle, Right]
host Middle Middle = uniform [Left, Right]
host Right  Right  = uniform [Left, Middle] 
host Left Middle = uniform [Right]
host Left Right = uniform [Middle]
host Middle Left = uniform [Right]
host Middle Right = uniform [Left]
host Right Left = uniform [Middle]
host Right Middle = uniform [Left]


montyFall :: Subdistribution Door
montyFall = do
  car <- uniform [Left,Middle,Right]
  choice <- uniform [Left,Middle,Right]
  announcement <- uniform [Left, Middle, Right]
  observe (choice == Middle)
  observe (announcement == Left)
  observe (car /= Left)
  return car

-- >>> montyFall
-- <Subdistribution>
-- Validity: 2 % 27
-- Posterior: [(Middle,1 % 2),(Right,1 % 2)]

