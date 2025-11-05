{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module Subdistribution where

import Prelude hiding ((>>=), (>>), return, map)
import Prelude qualified as P
import Data.Ord
import Data.Maybe
import Data.List ( maximumBy )
import Distribution qualified as D hiding ((>>=), (>>), return)
import Distribution (Distribution (..), distribution)
import Measure (measNormalize, condense, Measure (..))


newtype Subdistribution a = Subdistribution (Distribution (Maybe a))

byDefinition :: Subdistribution a -> Distribution (Maybe a)
byDefinition (Subdistribution s) = s

toList :: (Eq a) => Subdistribution a -> [(a, Rational)]
toList (Subdistribution d) = 
  P.map (\(x,v) -> (fromMaybe undefined x, v)) $ 
  filter (\(x,v) -> isJust x) $ 
  D.toList d

--fromList :: (Eq a) => [(a,Rational)] -> Subdistribution a
--fromList = Subdistribution . . condense
--  where

instance (Eq a) => Eq (Subdistribution a) where
  (==) (Subdistribution x) (Subdistribution y) = (x == y)

validity :: (Eq a) => Subdistribution a -> Rational
validity (Subdistribution d) = D.weightOf True (D.bind d (D.pure . isJust))

uniform :: (Eq a) => [a] -> Subdistribution a
uniform l = Subdistribution $ D.uniform $ P.map Just l

empty :: (Eq a) => Subdistribution a
empty = Subdistribution $ D.pure Nothing

-- instance (Eq a) => Eq (Subdistribution a) where
--    (==) :: Subdistribution a -> Subdistribution a -> Bool
--    (==) (Subdistribution u) (Subdistribution v) = 
--     isJust $ checkMaybe (condense u) (condense v)

(>>=) :: (Eq a, Eq b) => Subdistribution a -> (a -> Subdistribution b) -> Subdistribution b
(>>=) (Subdistribution d) f = Subdistribution $ D.bind d (byDefinition . fstar)
  where
    fstar (Just a) = f a
    fstar Nothing = empty

map :: (Eq a, Eq b) => (a -> b) -> Subdistribution a -> Subdistribution b
map f d = d >>= (return . f)

(>>) :: (Eq a, Eq b) => Subdistribution a -> Subdistribution b -> Subdistribution b
(>>) d f = d >>= const f

return :: (Eq a) => a -> Subdistribution a
return x = uniform [x]

pure :: (Eq a) => a -> Subdistribution a
pure = return

observe :: Bool -> Subdistribution ()
observe True = Subdistribution (D.pure (Just ()))
observe False = Subdistribution (D.pure Nothing)





instance (Eq a, Show a) => Show (Subdistribution a) where
  show :: Eq a => Subdistribution a -> String
  show = showSubdistribution
  
showSubdistribution :: (Eq a, Show a) => Subdistribution a -> String
showSubdistribution d =
  "<Subdistribution>\n" ++
  "Validity: " ++ show (validity d) ++ "\n" ++
  "Posterior: " ++ show (rescale d) ++ "\n"

rescale :: (Eq a) => Subdistribution a -> Distribution a
rescale xs = Distribution (Measure $ measNormalize $ toList xs)

--normFilter :: (Eq a) => (a -> Bool) -> Subdistribution a -> Subdistribution a
--normFilter p d = rescale (Subdistribution (filter (\(x,v) -> p x) (M.toList d)))

dJoin :: (Eq a) => Subdistribution (Subdistribution a) -> Subdistribution a
dJoin dss = (>>=) dss id