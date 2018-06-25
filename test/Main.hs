module Main where

import Test.Tasty
import Data.Proxy
import System.Environment

import Test.Tasty.QuickCheck.Laws



main :: IO ()
main = do
  setEnv "NUM_TASTY_THREADS" "4"
  defaultMain $ testGroup "Laws"
    [ testGroup "Eq Laws"
      [ testEqLaws pU
      , testEqLaws pB
      , testEqLaws pI
      ]
    , testGroup "Monoid Laws"
      [ testMonoidLaws (Proxy :: Proxy [()])
      , testMonoidLaws (Proxy :: Proxy [Bool])
      , testMonoidLaws (Proxy :: Proxy (Maybe [()]))
      ]
    , testGroup "Functor Laws"
      [ testFunctorLaws3 pMb pU pU pB pI (const (==))
      , testFunctorLaws3 pEi pU pU pB pI (const (==))
      , testFunctorLaws3 pLs pU pU pB pI (const (==))
      ]
    , testGroup "Applicative Laws"
      [ testApplicativeLaws3 pMb pU pU pB pI (const (==))
      , testApplicativeLaws3 pEi pU pU pB pI (const (==))
      , testApplicativeLaws3 pLs pU pU pB pI (const (==))
      ]
    , testGroup "Monad Laws"
      [ testMonadLaws3 pMb pU pU pB pI (const (==))
      , testMonadLaws3 pEi pU pU pB pI (const (==))
      , testMonadLaws3 pLs pU pU pB pI (const (==))
      ]
    ]



pMb = Proxy :: Proxy Maybe
pEi = Proxy :: Proxy (Either Int)
pLs = Proxy :: Proxy []

pU = Proxy :: Proxy ()
pI = Proxy :: Proxy Int
pB = Proxy :: Proxy Bool
