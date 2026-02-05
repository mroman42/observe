{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}


module MDB where


import AffineBag
import Distribution
import FinitaryMonad


newtype NormBag a = NormBag (Maybe (Distribution (Bag a)))