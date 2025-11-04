{-# LANGUAGE RebindableSyntax #-}

module ExampleNewcomb where

import Subdistribution
import Prelude hiding ((>>=), (>>), return)

data Action = OneBox | TwoBox deriving (Eq, Show) 

newcomb :: Action -> Distribution Double
newcomb x = do
    action <- uniform [OneBox, TwoBox]
    prediction <- uniform [OneBox, TwoBox]
    observe (action == prediction)
    observe (action == x)
    return $ case (action, prediction) of
        (OneBox, OneBox) -> 100
        (OneBox, TwoBox) -> 0
        (TwoBox, OneBox) -> 101
        (TwoBox, TwoBox) -> 1
