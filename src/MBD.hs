{-# LANGUAGE RebindableSyntax #-}

module MBD where

import Prelude hiding ((>>=), (>>), return, pure)
import qualified FinMonad.Bag as B
import FinMonad.AffineBag 
import FinMonad.Distribution hiding ((>>=), (>>), return)
import FinMonad.NormalizedDistribution (normalizing)
import DistributeMaybeAffinebag
import DistributeAffinebagDistribution

newtype MBD a = MBD (Maybe (AffineBag (Distribution a))) deriving (Eq, Show)

unMBD :: MBD a -> Maybe (AffineBag (Distribution a))
unMBD (MBD x) = x

mbdMult :: (Eq a) => MBD (MBD a) -> MBD a
mbdMult (MBD x) = MBD 
  $ fMap (fMap fJoin)
  $ fMap fJoin
  $ fMap (fMap distributeAffinebagDistribution)
  $ fJoin
  $ fMap distributiveMaybeBag
  $ fMap (fMap normalizing)
  $ fMap (fMap (fMap unMBD)) x

instance FinitaryMonad MBD where
  fMap :: (Eq a, Eq b) => (a -> b) -> MBD a -> MBD b
  fMap f (MBD x) = MBD $ fMap (fMap (fMap f)) x

  fBind :: (Eq a, Eq b) => MBD a -> (a -> MBD b) -> MBD b
  fBind a f = mbdMult $ fMap f a

  fReturn :: (Eq a) => a -> MBD a
  fReturn x = MBD (fReturn (fReturn (fReturn x)))

(>>=) :: (Eq a, Eq b) => MBD a -> (a -> MBD b) -> MBD b
(>>=) = fBind

(>>) :: (Eq a, Eq b) => MBD a -> MBD b -> MBD b
(>>) = fNext

return :: (Eq a) => a -> MBD a
return = fReturn

distribution :: (Eq a) => [(a, Rational)] -> MBD a
distribution d = MBD $ fReturn $ fReturn $ Distribution d

bag :: (Eq a) => [a] -> MBD a
bag l = MBD $ Just $ AffineBag $ B.bag $ map (\x -> Distribution [(x,1)]) l

observe :: Bool -> MBD ()
observe False = MBD $ Nothing
observe True  = MBD $ fReturn $ fReturn $ fReturn ()

