module Main where

import Test.Tasty
import Data.Proxy

import Test.Tasty.QuickCheck.Laws.Monad


main :: IO ()
main = defaultMain $
  testGroup "Laws"
    [ testGroup "Monad Laws"
      [ test_monad_laws_3 pMb pU pB pI (==)
      , test_monad_laws_3 pEi pU pB pI (==)
      , test_monad_laws_3 pLs pU pB pI (==)
      ]
    ]


pMb = Proxy :: Proxy Maybe
pEi = Proxy :: Proxy (Either Int)
pLs = Proxy :: Proxy []

pU = Proxy :: Proxy ()
pI = Proxy :: Proxy Int
pB = Proxy :: Proxy Bool
