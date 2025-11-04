{-# LANGUAGE RebindableSyntax #-}

module ExampleDamascus where

import Subdistribution
import Prelude hiding ((>>=), (>>), return)


deathInDamascus :: Strategy -> Distribution Outcome
deathInDamascus f = do
  merchant <- uniform [Fleeing, Staying, Random]
  death <- case merchant of
    Fleeing -> uniform [Aleppo]
    Staying -> uniform [Damascus]
    Random  -> uniform [Aleppo, Damascus]
  coin <- uniform [True, False]
  observe (f == merchant)
  return $ outcome (flee merchant coin) death
    
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