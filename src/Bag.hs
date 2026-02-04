{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}


module Bag
    ( 
    ) where

import Prelude hiding ((>>=), (>>), return, pure, map)
import Prelude qualified as P
import AuxiliarySemiring
import Data.Maybe
import FinitaryMonad

data Bag a where
    Bag :: (Eq a) => [(a, Int)] -> Bag a

unBag :: Bag a -> [(a, Int)]    
unBag (Bag d) = d

bag :: (Eq a) => [(a, Int)] -> Bag a
bag = Bag

instance (Eq a) => Eq (Bag a) where
  (==) :: (Eq a) => Bag a -> Bag a -> Bool
  (==) (Bag m) (Bag n) = isJust $ checkMaybe m n

bagReturn :: (Eq a) => a -> Bag a
bagReturn x = Bag [(x,1)]


instance FinitaryMonad Bag where
  fBind :: (Eq a, Eq b) => Bag a -> (a -> Bag b) -> Bag b
  fBind (Bag d) f = Bag $ sBind d (unBag . f)

  fReturn x = Bag [(x,1)]

  fNext :: (Eq a, Eq b) => Bag a -> Bag b -> Bag b
  fNext d f = fBind d (const f)

  fMap :: (Eq a, Eq b) => (a -> b) -> Bag a -> Bag b
  fMap f (Bag u) = Bag $ sMap f u