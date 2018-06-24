module Main where

import Test.Tasty
import Data.Proxy

import Test.Tasty.QuickCheck.Laws.Functor
import Test.Tasty.QuickCheck.Laws.Monad


main :: IO ()
main = defaultMain $
  testGroup "Laws"
    [ testGroup "Functor Laws"
      [ testFunctorLaws3 pMb pU pB pI (==)
      ]
    , testGroup "Monad Laws"
      [ testMonadLaws3 pMb pU pB pI (==)
      , testMonadLaws3 pEi pU pB pI (==)
      , testMonadLaws3 pLs pU pB pI (==)
      ]
    ]


pMb = Proxy :: Proxy Maybe
pEi = Proxy :: Proxy (Either Int)
pLs = Proxy :: Proxy []

pU = Proxy :: Proxy ()
pI = Proxy :: Proxy Int
pB = Proxy :: Proxy Bool
