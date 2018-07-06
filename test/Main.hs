module Main where

import Test.Tasty
import Data.Proxy
import System.Environment
import Control.Monad (ap)
import Test.QuickCheck

import Test.Tasty.QuickCheck.Laws



main :: IO ()
main = do
  setEnv "NUM_TASTY_THREADS" "6"
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
      , testStateMonadLaws pSB pB pB pB stateEq get put
      , testStateMonadLaws pSI pI pI pI stateEq get put
      ]
    , testGroup "Reader Monad Laws"
      [ testReaderMonadLaws pRU pU pU pU pU readerEq ask local
      , testReaderMonadLaws pRB pB pB pB pB readerEq ask local
      , testReaderMonadLaws pRI pI pI pI pI readerEq ask local
      ]
    , testGroup "Error Monad Laws"
      [ testErrorMonadLaws pEU pU pU pU pU (const (==)) throw catch
      , testErrorMonadLaws pEB pU pB pU pU (const (==)) throw catch
      , testErrorMonadLaws pEI pU pI pU pU (const (==)) throw catch
      ]
    , testGroup "Writer Monad Laws"
      [ testWriterMonadLaws pWU pU pU pU (const (==)) tell consume edit
      , testWriterMonadLaws (pWLs pB) pU (pLs' pB) pU (const (==)) tell consume edit
      , testWriterMonadLaws (pWLs pI) pU (pLs' pI) pU (const (==)) tell consume edit
      ]
    ]



pU = Proxy :: Proxy ()
pB = Proxy :: Proxy Bool
pI = Proxy :: Proxy Int

pMb = Proxy :: Proxy Maybe
pEi = Proxy :: Proxy (Either Int)
pLs = Proxy :: Proxy []

pLs' = const Proxy :: Proxy a -> Proxy [a]

pSU = Proxy :: Proxy (State ())
pSB = Proxy :: Proxy (State Bool)
pSI = Proxy :: Proxy (State Int)

pRU = Proxy :: Proxy (Reader ())
pRB = Proxy :: Proxy (Reader Bool)
pRI = Proxy :: Proxy (Reader Int)

pEU = Proxy :: Proxy (Error ())
pEB = Proxy :: Proxy (Error Bool)
pEI = Proxy :: Proxy (Error Int)

pWU = Proxy :: Proxy (Writer ())
pWLs = const Proxy :: Proxy a -> Proxy (Writer [a])



-- Basic State Monad

data State s a = State
  { runState :: s -> (a,s) }

stateEq
  :: (Eq s, Eq a)
  => s -> State s a -> State s a -> Bool
stateEq s x y = (runState x s) == (runState y s)

instance Monad (State s) where
  return a = State $ \s -> (a,s)

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



-- Basic Reader Monad

data Reader r a = Reader
  { runReader :: r -> a }

readerEq
  :: (Eq a)
  => r -> Reader r a -> Reader r a -> Bool
readerEq r x y = (runReader x r) == (runReader y r)

instance Monad (Reader r) where
  return a = Reader $ \_ -> a

  x >>= f = Reader $ \r ->
    let a = runReader x r
    in runReader (f a) r

instance Applicative (Reader r) where
  pure = return
  (<*>) = ap

instance Functor (Reader r) where
  fmap f x = x >>= (return . f)

instance (Arbitrary a) => Arbitrary (Reader r a) where
  arbitrary = return <$> arbitrary

instance Show (Reader r a) where
  show _ = "<Reader>"

ask :: Reader r r
ask = Reader $ \r -> r

local :: (r -> r) -> Reader r a -> Reader r a
local u x = Reader $ \r -> runReader x (u r)



-- Basic Error Monad

data Error e a = Error
  { runError :: Either e a
  } deriving (Eq, Show)

instance Monad (Error e) where
  return a = Error (Right a)

  x >>= f = case x of
    Error (Left e) -> Error (Left e)
    Error (Right a) -> f a

instance Applicative (Error e) where
  pure = return
  (<*>) = ap

instance Functor (Error e) where
  fmap f x = x >>= (return . f)

instance (Arbitrary a, Arbitrary e) => Arbitrary (Error e a) where
  arbitrary = Error <$> arbitrary

throw :: e -> Error e a
throw e = Error (Left e)

catch :: Error e a -> (e -> Error e a) -> Error e a
catch x h = case x of
  Error (Right a) -> Error (Right a)
  Error (Left e) -> h e



-- Basic Writer Monad

data Writer w a = Writer
  { runWriter :: (a,w)
  } deriving (Eq, Show)

instance (Monoid w) => Monad (Writer w) where
  return a = Writer (a, mempty)

  x >>= f =
    let
      (a,w1) = runWriter x
      (b,w2) = runWriter (f a)
    in
      Writer (b, mappend w1 w2)

instance (Monoid w) => Applicative (Writer w) where
  pure = return
  (<*>) = ap

instance (Monoid w) => Functor (Writer w) where
  fmap f x = x >>= (return . f)

instance (Arbitrary w, Arbitrary a) => Arbitrary (Writer w a) where
  arbitrary = Writer <$> arbitrary

tell :: w -> Writer w ()
tell w = Writer ((),w)

consume :: (Monoid w) => Writer w a -> Writer w (a,w)
consume x =
  let (a,w) = runWriter x
  in Writer ((a,w), mempty)

edit :: Writer w (w -> w) -> Writer w a -> Writer w a
edit u x =
  let
    (f,_) = runWriter u
    (a,w) = runWriter x
  in
    Writer (a, f w)
