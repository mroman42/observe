{-# LANGUAGE RebindableSyntax #-}

module ExampleHalpern where

import Prelude hiding (id, (.), (>>=), (>>), return, Left, Right)
import Subdistribution


data Colour = Red | Blue | Green | Yellow deriving (Show, Eq)
data Shade = Dark | Light deriving (Show, Eq)

dimly :: Colour -> Shade
dimly Red = Light
dimly Yellow = Light
dimly Blue = Dark
dimly Green = Dark

prior :: Subdistribution Colour
prior = subdistribution [(Red, 1/5), (Blue, 1/5), (Green, 1/5), (Yellow, 2/5)]

experimentPearl :: Subdistribution Colour
experimentPearl = do
  s <- subdistribution [(Dark, 7/10), (Light, 3/10)]
  x <- prior
  observe (dimly x == s)
  return x

experimentJeffrey :: Subdistribution Colour
experimentJeffrey = do
  x1 <- prior
  x2 <- prior
  observe (dimly x1 == Dark)
  observe (dimly x2 == Light)
  x <- subdistribution [(x1, 7/10), (x2, 3/10)]
  return x

procedure :: Shade -> Subdistribution Colour
procedure s = do
  x <- prior
  observe (dimly x == s)
  return x

experimentPearl2 :: Subdistribution Colour
experimentPearl2 = do
  s <- subdistribution [(Dark, 7/10), (Light, 3/10)]
  procedure s

experimentJeffrey2 :: Subdistribution Colour
experimentJeffrey2 = do
  x1 <- procedure Dark
  x2 <- procedure Light
  x <- subdistribution [(x1, 7/10), (x2, 3/10)]
  return x
