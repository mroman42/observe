{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}


module Distribution 
    ( Distribution (..)
    , distribution
    , unDistribution
    , uniform
    , weightOf
    , (>>=)
    , (>>)
    , return
    , squeezeDist
    ) where

import Prelude hiding ((>>=), (>>), return, pure)
import FinitaryMonad
import AuxiliarySemiring
import Data.Maybe
import GHC.RTS.Flags (DebugFlags(squeeze))

data Distribution a where
    Distribution :: (Eq a) => [(a, Rational)] -> Distribution a

unDistribution :: Distribution a -> [(a, Rational)]
unDistribution (Distribution d) = d

distribution :: (Eq a) => [(a, Rational)] -> Distribution a
distribution d = case totalWeight d of
    1 -> Distribution $ condense d
    _ -> error "Non-full support for a distribution."

weightOf :: a -> Distribution a -> Rational
weightOf x (Distribution m) = weightOfPoint x m

uniform :: (Eq a) => [a] -> Distribution a
uniform l = Distribution $ map (\x -> (x, 1 / (fromIntegral (length l)))) l


instance (Eq a) => Eq (Distribution a) where
    (==) :: Distribution a -> Distribution a -> Bool
    (==) (Distribution a) (Distribution b) = isJust $ checkMaybe a b


instance FinitaryMonad Distribution where
    fBind :: (Eq a, Eq b) 
      => Distribution a -> (a -> Distribution b) -> Distribution b
    fBind (Distribution d) f = 
      Distribution $ sBind d (unDistribution . f)

    fNext :: (Eq a, Eq b) => Distribution a -> Distribution b -> Distribution b
    fNext d f = fBind d (const f)

    fReturn :: (Eq a) => a -> Distribution a
    fReturn x = uniform [x]

    fMap :: (Eq a, Eq b) => (a -> b) -> Distribution a -> Distribution b
    fMap f (Distribution d) = Distribution $ condense (sMap f d)

(>>=) :: (Eq a, Eq b) 
  => Distribution a -> (a -> Distribution b) -> Distribution b
(>>=) = fBind

(>>) :: (Eq a, Eq b) 
  => Distribution a -> Distribution b -> Distribution b
(>>) = fNext

return :: (Eq a) => a -> Distribution a
return = fReturn

pure :: (Eq a) => a -> Distribution a
pure = return

instance (Eq a, Show a) => Show (Distribution a) where
  show :: (Eq a) => Distribution a -> String
  show (Distribution d) = "<Distribution> " ++ show d

squeezeDist :: (Eq a) => a -> Rational -> Distribution a -> Distribution a
squeezeDist x r (Distribution d) =
  Distribution $ (x,r) : valueMap (\s -> s / (1-r)) d