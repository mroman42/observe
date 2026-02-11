{-# LANGUAGE RebindableSyntax #-}

module ExampleDamascus where

import FinMonad.Subdistribution
import Prelude hiding ((>>=), (>>), return)

-- Cheating Death in Damascus with a random coin.

deathInDamascus :: Strategy -> Subdistribution Outcome
deathInDamascus f = do
  merchant <- uniform [Fleeing, Staying, Random]
  death <- case merchant of
    Fleeing -> uniform [Aleppo]
    Staying -> uniform [Damascus]
    Random  -> uniform [Aleppo, Damascus]
  coin <- uniform [True, False]
  observe (f == merchant)
  return $ outcome (flee merchant coin) death

-- >>> deathInDamascus Fleeing
-- <Subdistribution>
-- Validity: 1 % 3
-- Posterior: <Distribution> [(MerchantTravelsAndMeetsDeath,1 % 1)]

-- >>> deathInDamascus Staying
-- <Subdistribution>
-- Validity: 1 % 3
-- Posterior: <Distribution> [(MerchantMeetsDeath,1 % 1)]
    
-- >>> deathInDamascus Random
-- <Subdistribution>
-- Validity: 1 % 3
-- Posterior: <Distribution> [(MerchantTravelsAndMeetsDeath,1 % 4),(MerchantEscapes,1 % 4),(MerchantTravelsAndEscapes,1 % 4),(MerchantMeetsDeath,1 % 4)]

flee :: Strategy -> Bool -> City
flee Fleeing _ = Aleppo
flee Staying _ = Damascus
flee Random True = Aleppo
flee Random False = Damascus 

outcome :: City -> City -> Outcome
outcome Aleppo Aleppo     = MerchantTravelsAndMeetsDeath
outcome Aleppo Damascus   = MerchantTravelsAndEscapes
outcome Damascus Aleppo   = MerchantEscapes
outcome Damascus Damascus = MerchantMeetsDeath

data Strategy = Fleeing | Staying | Random deriving (Eq, Show) 
data City = Damascus | Aleppo deriving (Eq,Show) 

data Outcome = 
      MerchantTravelsAndMeetsDeath 
    | MerchantTravelsAndEscapes
    | MerchantEscapes
    | MerchantMeetsDeath
  deriving (Eq, Show) 
