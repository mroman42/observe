{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module FinMonad.Subdistribution where

import Prelude hiding ((>>=), (>>), return, map)
import Prelude qualified as P
import Data.Ord
import Data.Maybe
--import Data.List ( maximumBy )
import FinMonad.FinMonad
import FinMonad.Distribution qualified as D hiding ((>>=), (>>), return)
import FinMonad.Distribution (Distribution (..), distribution)
import Measure (measNormalize, totalWeight, Measure (..))
import AuxiliarySemiring
import qualified GHC.TypeLits as D


newtype Subdistribution a = Subdistribution (Distribution (Maybe a))

toDistribution :: Subdistribution a -> Distribution (Maybe a)
toDistribution (Subdistribution s) = s

toList :: (Eq a) => Subdistribution a -> [(a, Rational)]
toList (Subdistribution d) = 
  P.map (\(x,v) -> (fromMaybe undefined x, v)) $ 
  filter (\(x,v) -> isJust x) $ 
  D.unDistribution d

subdistribution :: (Eq a) => [(a,Rational)] -> Subdistribution a
subdistribution = fromList

fromList :: (Eq a) => [(a,Rational)] -> Subdistribution a
fromList l = Subdistribution $ Distribution $ lm
  where
    weightNothing = 1 - totalWeight l
    lm = (Nothing, weightNothing) : P.map (\(x,v) -> (Just x, v)) l


instance (Eq a) => Eq (Subdistribution a) where
  (==) (Subdistribution x) (Subdistribution y) = (x == y)

validity :: (Eq a) => Subdistribution a -> Rational
validity (Subdistribution d) = D.weightOf True (fBind d (fReturn . isJust))

uniform :: (Eq a) => [a] -> Subdistribution a
uniform l = Subdistribution $ D.uniform $ P.map Just l

empty :: (Eq a) => Subdistribution a
empty = Subdistribution $ fReturn Nothing

(>>=) :: (Eq a, Eq b) => Subdistribution a -> (a -> Subdistribution b) -> Subdistribution b
(>>=) (Subdistribution d) f = Subdistribution $ fBind d (toDistribution . fstar)
  where
    fstar (Just a) = f a
    fstar Nothing = empty

map :: (Eq a, Eq b) => (a -> b) -> Subdistribution a -> Subdistribution b
map f d = d >>= (return . f)

(>>) :: (Eq a, Eq b) => Subdistribution a -> Subdistribution b -> Subdistribution b
(>>) d f = d >>= const f

(<%>) :: (Eq a, Eq b) => (a -> b) -> (Subdistribution a -> Subdistribution b)
f <%> d = map f d


return :: (Eq a) => a -> Subdistribution a
return x = uniform [x]

pure :: (Eq a) => a -> Subdistribution a
pure = return

observe :: Bool -> Subdistribution ()
observe True = Subdistribution (fReturn (Just ()))
observe False = Subdistribution (fReturn Nothing)





instance (Eq a, Show a) => Show (Subdistribution a) where
  show :: Eq a => Subdistribution a -> String
  show = showSubdistribution
  
showSubdistribution :: (Eq a, Show a) => Subdistribution a -> String
showSubdistribution d =
  "<Subdistribution>\n" ++
  "Validity: " ++ show (validity d) ++ "\n" ++
  "Posterior: " ++ show (rescale d) ++ "\n"

rescale :: (Eq a) => Subdistribution a -> Distribution a
rescale xs = Distribution (measNormalize $ toList xs)

dJoin :: (Eq a) => Subdistribution (Subdistribution a) -> Subdistribution a
dJoin dss = (>>=) dss id
