{-# LANGUAGE RebindableSyntax #-}

module Sequencing where

import Prelude hiding ((>>=), (>>), return, fmap)
import FinMonad.FinMonad

(>>=) :: (FinitaryMonad m, Eq a, Eq b) => m a -> (a -> m b) -> m b
(>>=) = fBind

return :: (FinitaryMonad m, Eq a) => a -> m a
return = fReturn

fmap :: (FinitaryMonad m, Eq a, Eq b) => (a -> b) -> m a -> m b
fmap = fMap

rSeq :: (FinitaryMonad m, Eq a) => [m a] -> m [a]
rSeq [] = fReturn []
rSeq (m : l) = do
    x <- m
    y <- rSeq l
    return (x : y)

lSeq :: (FinitaryMonad m, Eq a) => [m a] -> m [a]
lSeq [] = return []
lSeq l = do
  y <- lSeq (init l)
  x <- last l
  return (y ++ [x])
