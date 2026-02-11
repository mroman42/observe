{-# LANGUAGE RebindableSyntax #-}

module MDB where

import Prelude hiding ((>>=), (>>), return, pure)
import AffineBag 
import qualified Bag as B
import Distribution hiding ((>>=), (>>), return)
import FinitaryMonad
import NormalizedDistribution (normalizing)
import DistributeMaybeAffinebag
import AuxiliarySemiring (sMap)

newtype MDB a = MDB (Maybe (Distribution (AffineBag a))) deriving (Eq, Show)

unMDB :: MDB a -> Maybe (Distribution (AffineBag a))
unMDB (MDB x) = x

mdbMult :: (Eq a) => MDB (MDB a) -> MDB a
mdbMult (MDB x) = MDB
  $ fMap fJoin
  $ fMap (fMap (fMap fJoin))
  $ fMap (fMap distributeBagDist)
  $ fJoin
  $ fMap normalizing
  $ fMap (fMap distributiveMaybeBag)
  $ fMap (fMap (fMap unMDB)) x

instance FinitaryMonad MDB where
  fMap :: (Eq a, Eq b) => (a -> b) -> MDB a -> MDB b
  fMap f (MDB x) = MDB $ fMap (fMap (fMap f)) x

  fBind :: (Eq a, Eq b) => MDB a -> (a -> MDB b) -> MDB b
  fBind a f = mdbMult $ fMap f a

  fNext :: (Eq a, Eq b) => MDB a -> MDB b -> MDB b
  fNext d f = fBind d (const f)

  fReturn :: (Eq a) => a -> MDB a
  fReturn x = MDB (fReturn (fReturn (fReturn x)))

(>>=) :: (Eq a, Eq b) => MDB a -> (a -> MDB b) -> MDB b
(>>=) = fBind

(>>) :: (Eq a, Eq b) => MDB a -> MDB b -> MDB b
(>>) = fNext

return :: (Eq a) => a -> MDB a
return = fReturn


distribution :: (Eq a) => [(a, Rational)] -> MDB a
distribution d = MDB $ fReturn $ Distribution $ sMap fReturn d

observe :: Bool -> MDB ()
observe False = MDB $ Nothing
observe True  = MDB $ fReturn $ fReturn $ fReturn ()

bag :: (Eq a) => [a] -> MDB a
bag l = MDB $ fReturn $ fReturn $ AffineBag $ B.bag l