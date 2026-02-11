{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}


module MaybeDistBag where

import Prelude hiding ((>>=), (>>), return)
import Distribution hiding ((>>=), (>>), return, uniform, distribution)
import Subdistribution hiding ((>>=), (>>), return)
import AffineBag
import FinitaryMonad
import Data.Maybe
import NormalizedDistribution hiding ((>>=), (>>), return)

newtype NormBag a = NormBag (Maybe (Distribution (AffineBag a)))

unNormBag :: NormBag a -> Maybe (Distribution (AffineBag a))
unNormBag (NormBag x) = x

instance (Eq a) => Eq (NormBag a) where
    (==) :: (Eq a) => NormBag a -> NormBag a -> Bool
    (==) (NormBag xs) (NormBag ys) = xs == ys

instance (Eq a, Show a) => Show (NormBag a) where
    show :: (Show a) => NormBag a -> String
    show (NormBag xs) = show xs

(>>=) :: (Eq a, Eq b) => NormBag a -> (a -> NormBag b) -> NormBag b
(>>=) (NormBag Nothing) f = NormBag Nothing
(>>=) (NormBag (Just d)) f = NormBag 
    $ fmap (fMap fJoin)
    $ fmap fJoin
    $ fmap (fMap distributeBagDist)
    $ toMaybeDistribution . normalize . Subdistribution
    $ fMap distributiveMaybeBag
    $ fMap (fMap (unNormBag . f)) d

(>>) :: (Eq a, Eq b) => NormBag a -> NormBag b -> NormBag b
(>>) d f = d >>= const f

return :: (Eq a) => a -> NormBag a
return = NormBag . Just . fReturn . fReturn