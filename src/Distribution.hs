{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}


module Distribution 
    ( uniform
    , (>>=)
    , Distribution (..)
    ) where


import Prelude hiding ((>>=), return)
import SubdistributionAux
import Subdistribution qualified as Sub
import Subdistribution (unSubdistribution)


data Distribution a where
    Distribution :: (Eq a) => Sub.Subdistribution a -> Distribution a

distToSub :: Distribution a -> Sub.Subdistribution a
distToSub (Distribution a) = a

fromSubdistribution :: (Eq a) => Sub.Subdistribution a -> Distribution a    
fromSubdistribution d = case Sub.validity d == 1 of 
    True -> Distribution d 
    False -> error "This is a non-total distribution"

uniform :: (Eq a) => [a] -> Distribution a
uniform = Distribution . Sub.uniform

(>>=) :: (Eq a, Eq b) => Distribution a -> (a -> Distribution b) -> Distribution b
(>>=) (Distribution d) f = Distribution $ d Sub.>>= (distToSub . f)

instance (Eq a, Show a) => Show (Distribution a) where
  show :: (Eq a) => Distribution a -> String
  show (Distribution d) = "<Distribution>\n" ++ show (Sub.unSubdistribution d)