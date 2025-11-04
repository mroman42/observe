{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module Subdistribution where

import Prelude hiding ((>>=), return)
import Data.Ord
import Data.Maybe
import Data.List ( maximumBy )
import GHC.Generics (Generic)
import SubdistributionAux

data Subdistribution a where
  Subdistribution :: (Eq a) => [(a, Rational)] -> Subdistribution a

unSubdistribution :: Subdistribution a -> [(a, Rational)]
unSubdistribution (Subdistribution d) = d

validity :: Subdistribution a -> Rational
validity (Subdistribution l) = totalWeight l

instance (Eq a) => Eq (Subdistribution a) where
  (==) :: Subdistribution a -> Subdistribution a -> Bool
  (==) (Subdistribution u) (Subdistribution v) = 
    isJust $ checkMaybe (condense u) (condense v)

(>>=) :: (Eq a, Eq b) => Subdistribution a -> (a -> Subdistribution b) -> Subdistribution b
(>>=) (Subdistribution d) f = Subdistribution $ distBind d (unSubdistribution . f)

dmap :: (Eq a, Eq b) => (a -> b) -> Subdistribution a -> Subdistribution b
dmap f d = d >>= (return . f)

(>>) :: (Eq a, Eq b) => 
  Subdistribution a -> Subdistribution b -> Subdistribution b
(>>) d f = d >>= const f

return :: (Eq a) => a -> Subdistribution a
return x = Subdistribution [(x,1)]


observe :: Bool -> Subdistribution ()
observe True = return ()
observe False = absurd

assert :: Bool -> Subdistribution ()
assert = observe

absurd :: (Eq a) => Subdistribution a
absurd = Subdistribution []

fromList, distribution :: (Eq a) => [(a,Rational)] -> Subdistribution a
fromList = Subdistribution . condense
distribution = fromList

toList :: (Eq a) => Subdistribution a -> [(a,Rational)]
toList = unSubdistribution

instance (Eq a, Show a) => Show (Subdistribution a) where
  show :: Eq a => Subdistribution a -> String
  show = showSubdistribution
  
showSubdistribution :: (Eq a, Show a) => Subdistribution a -> String
showSubdistribution d =
  "<Subdistribution>\n" ++
  "Validity: " ++ show (validity d) ++ "\n" ++
  "Posterior: " ++ show (unSubdistribution (rescale d)) ++ "\n"

rescale :: Subdistribution a -> Subdistribution a
rescale (Subdistribution x) = Subdistribution (distNormalize x)

uniform :: (Eq a) => [a] -> Subdistribution a
uniform l = fromList (map (, 1 / toRational (length l)) l)

weightOf :: (Eq a) => a -> Subdistribution a -> Rational
weightOf x (Subdistribution d) = weightOfPoint x d

normFilter :: (Eq a) => (a -> Bool) -> Subdistribution a -> Subdistribution a
normFilter p d = rescale (Subdistribution (filter (\(x,v) -> p x) (toList d)))

dJoin :: (Eq a) => Subdistribution (Subdistribution a) -> Subdistribution a
dJoin dss = (>>=) dss id