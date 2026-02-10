{-# LANGUAGE RebindableSyntax #-}
module DistBag where

import Prelude hiding ((>>=), (>>), return, pure, map)
import qualified Distribution as D hiding ((>>=), (>>), return)
import Distribution (Distribution(..))
import Bag hiding ((>>=), (>>), return)
import FinitaryMonad

pack :: (Eq a) => Distribution (Bag a) -> Bag (Distribution a)
pack (Distribution d) = fMap Distribution (listPack d)

listPack :: (Eq a) => [((Bag a), Rational)] -> Bag [(a,Rational)]
listPack [] = bag [[]]
listPack ((b,p):l) = 
  fBind b (\x -> 
    fBind (listPack l) (\xs -> 
      fReturn ((x,p):xs)))
  --x <- b 
  --xs <- listPack l
  --return ((x,p):xs)


ex1 :: Distribution (Bag Char)
ex1 = D.distribution [(bag ['x'],2/3), (bag ['y'], 1/3)]

t1 :: Bool
t1 = pack (fMap fReturn $ ex1) == fReturn ex1
  
ex2 :: Bag Char
ex2 = bag ['x','y','z','x','y']

t2 :: Bool
t2 = (pack $ fReturn ex2) == fMap fReturn ex2

ex3 :: Distribution (Bag (Bag Char))
ex3 = D.distribution
  [ (bag [bag ['x','y'], bag['y'] ], 1/5)
  , (bag [bag ['x'], bag['y','x','z'] ], 1/5)
  , (bag [bag ['x','z']], 2/5)
  , (bag [], 1/5)
  ]

t3 :: Bool
t3 = (pack $ fMap fJoin ex3) == (fJoin $ fMap pack $ pack ex3)


newtype BagStoch a = BagStoch (Bag (Distribution a))
  deriving (Eq, Show)

unBagStoch :: BagStoch a -> (Bag (Distribution a))
unBagStoch (BagStoch x) = x

(>>=) :: (Eq a, Eq b) => BagStoch a -> (a -> BagStoch b) -> BagStoch b
(>>=) (BagStoch x) f = BagStoch
  $ fMap fJoin
  $ fJoin
  $ fMap pack
  $ fMap (fMap (unBagStoch . f)) x

(>>) :: (Eq a, Eq b) => BagStoch a -> BagStoch b -> BagStoch b
(>>) d f = d >>= const f

return :: (Eq a) => a -> BagStoch a
return = BagStoch . fReturn . fReturn

distribution :: (Eq a) => [(a,Rational)] -> BagStoch a
distribution = BagStoch . fReturn . D.distribution

observe :: Bool -> BagStoch ()
observe True  = BagStoch $ bag [D.distribution [((), 1)]]
observe False = BagStoch $ bag []