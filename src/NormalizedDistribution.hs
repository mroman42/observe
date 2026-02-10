{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE QualifiedDo #-}

module NormalizedDistribution where

import Subdistribution qualified as S hiding ((>>=), (>>), return)
import Distribution hiding ((>>=), (>>), return, uniform, distribution)
import Distribution qualified as D
import Prelude hiding ((>>=), (>>), return)
import Data.Maybe
import Subdistribution (Subdistribution)
import FinitaryMonad


newtype Normalized a = Normalized (Maybe (Distribution a))

unNormalized :: Normalized a -> Maybe (Distribution a)
unNormalized (Normalized d) = d

toMaybeDistribution :: Normalized a -> Maybe (Distribution a)
toMaybeDistribution (Normalized d) = d

toList :: (Eq a) => Normalized a -> [(a, Rational)]
toList (Normalized Nothing) = []
toList (Normalized (Just d)) = unDistribution d

ifThenElse True  x y = x
ifThenElse False x y = y


unsafeFromMaybe :: Maybe a -> a
unsafeFromMaybe = fromMaybe undefined

normalize :: (Eq a) => Subdistribution a -> Normalized a
normalize xs = case S.validity xs == 0 of
    True -> Normalized Nothing
    False -> Normalized $ Just $ S.rescale xs

normalizing :: (Eq a) => Distribution (Maybe a) -> Maybe (Distribution a)
normalizing x = unNormalized $ normalize $ S.Subdistribution x

instance (Eq a) => Eq (Normalized a) where
    (==) :: (Eq a) => Normalized a -> Normalized a -> Bool
    (==) (Normalized xs) (Normalized ys) = xs == ys

instance (Eq a, Show a) => Show (Normalized a) where
    show :: (Show a) => Normalized a -> String
    show (Normalized xs) = show xs

(>>=) :: (Eq a, Eq b) => Normalized a -> (a -> Normalized b) -> Normalized b
(>>=) (Normalized Nothing) f = Normalized Nothing
(>>=) (Normalized (Just d)) f = Normalized
    $ fmap fJoin
    $ toMaybeDistribution
    $ normalize
    $ S.Subdistribution
    $ fMap toMaybeDistribution 
    $ fMap f d

(>>) :: (Eq a, Eq b) => Normalized a -> Normalized b -> Normalized b
(>>) d f = d >>= const f

intervene :: Bool -> Normalized ()
intervene True = uniform [()]
intervene False = uniform []

observe = intervene

uniform :: (Eq a) => [a] -> Normalized a
uniform xs = Normalized $ Just $ D.uniform xs

return :: (Eq a) => a -> Normalized a
return x = uniform [x]

distribution :: (Eq a) => [(a, Rational)] -> Normalized a
distribution [] = Normalized Nothing
distribution xs = Normalized $ Just $ D.distribution xs
