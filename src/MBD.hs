{-# LANGUAGE RebindableSyntax #-}

module MBD where

import Prelude hiding ((>>=), (>>), return, pure, map)
import AffineBag
import Distribution hiding ((>>=), (>>), return)
import FinitaryMonad
import NormalizedDistribution (normalizing)

newtype MBD a = MBD (Maybe (AffineBag (Distribution a))) deriving (Eq, Show)

unMBD :: MBD a -> Maybe (AffineBag (Distribution a))
unMBD (MBD x) = x

mbdMult :: (Eq a) => MBD (MBD a) -> MBD a
mbdMult (MBD x) = MBD 
  $ _
-- $ fMap (fMap )
  $ fJoin
  $ fMap distributiveMaybeBag
  $ fMap (fMap normalizing)
  $ fMap (fMap (fMap unMBD)) x
