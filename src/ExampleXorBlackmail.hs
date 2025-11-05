{-# LANGUAGE RebindableSyntax #-}

module ExampleXorBlackmail where

import Prelude hiding ((>>=), (>>), return)
import Subdistribution
import Data.MultiSet qualified as MSet


multiset :: (Ord a) => [a] -> MSet.MultiSet a
multiset = MSet.fromList

isIn :: (Ord a) => a -> MSet.MultiSet a -> Bool
isIn = MSet.member


xorBlackmail :: Strategy -> Subdistribution Termites
xorBlackmail strategy = do
  termites <- uniform [Infested, Clean]
  prediction <- case strategy of
    Pay -> distribution [((Pay,Clean), 9/10), ((Refuse,Infested), 1/10)]
    Refuse -> distribution [((Refuse,Infested), 9/10), ((Pay,Infested), 1/20), ((Refuse,Clean), 1/20)]
  -- observe (fst predictor == strategy)
  -- observe (snd predictor == termites)
  observe (prediction `isIn` multiset [(Pay, Clean), (Refuse, Infested)])
  return termites

data Termites = Infested | Clean deriving (Eq, Show, Ord)  
data Strategy = Pay | Refuse deriving (Eq, Show, Ord)



-- >>> xorBlackmail Refuse
-- <Subdistribution>
-- Validity: 9 % 10
-- Posterior: [(Infested,1 % 2),(Clean,1 % 2)]

-- >>> xorBlackmail Pay
-- <Subdistribution>
-- Validity: 1 % 1
-- Posterior: [(Infested,1 % 2),(Clean,1 % 2)]
