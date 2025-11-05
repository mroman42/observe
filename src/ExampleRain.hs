{-# LANGUAGE RebindableSyntax #-}

module ExampleRain where

import Prelude hiding ((>>=), (>>), return)
import Distribution    

data Weather = Rainy | Sunny deriving (Eq, Show)
data Streets = Dry | Wet deriving (Eq, Show)
data Umbrella = Yes | No deriving (Eq, Show)

weather :: Distribution Weather
weather = distribution [(Rainy, 1/3), (Sunny, 2/3)]

streets :: Weather -> Distribution Streets
streets Rainy = distribution [(Dry, 1/10), (Wet, 9/10)]
streets Sunny = distribution [(Dry, 8/10), (Wet, 2/10)]

umbrella :: Distribution Umbrella
umbrella = do
    today <- weather
    lookOutside <- streets today
    case lookOutside of
        Dry -> distribution [(Yes, 1/8), (No, 7/8)]
        Wet -> distribution [(Yes, 7/8), (No, 1/8)]


-- >>> umbrella        
