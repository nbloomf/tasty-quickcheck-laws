module Main where

import Test.Tasty
import Data.Proxy
import System.Environment
import Control.Monad (ap)
import Test.QuickCheck

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
    , testGroup "State Monad Laws"
      [ testStateMonadLaws pSU pU pU pU stateEq get put
      , testStateMonadLaws pSI pI pI pI stateEq get put
      , testStateMonadLaws pSB pB pB pB stateEq get put
      ]
    ]



pMb = Proxy :: Proxy Maybe
pEi = Proxy :: Proxy (Either Int)
pLs = Proxy :: Proxy []

pU = Proxy :: Proxy ()
pI = Proxy :: Proxy Int
pB = Proxy :: Proxy Bool

pSU = Proxy :: Proxy (State ())
pSI = Proxy :: Proxy (State Int)
pSB = Proxy :: Proxy (State Bool)



-- Basic State Monad

data State s a = State
  { runState :: s -> (a,s) }

stateEq
  :: (Eq s, Eq a)
  => s -> State s a -> State s a -> Bool
stateEq s x y = (runState x s) == (runState y s)

instance Monad (State s) where
  return x = State $ \s -> (x,s)

  x >>= f = State $ \s1 ->
    let (a,s2) = runState x s1
    in runState (f a) s2

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Functor (State s) where
  fmap f x = x >>= (return . f)

instance (Arbitrary a) => Arbitrary (State s a) where
  arbitrary = return <$> arbitrary

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ \_ -> ((),s)
