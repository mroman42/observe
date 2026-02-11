module BagSequencing where

import Prelude hiding ((>>=), (>>), return, fmap)
import FinMonad.FinMonad
import FinMonad.Bag
import Sequencing


brSeq :: (FinitaryMonad m, Eq a) => Bag (m a) -> m (Bag a)
brSeq = fMap bag . rSeq . toList

blSeq :: (FinitaryMonad m, Eq a) => Bag (m a) -> m (Bag a)
blSeq = fMap bag . lSeq . toList
