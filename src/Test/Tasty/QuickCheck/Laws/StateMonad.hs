{- |
Module      : Test.Tasty.QuickCheck.Laws.StateMonad
Description : Prefab tasty trees of quickcheck properties for the state monad laws
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

State axioms taken from Gibbons and Hinze, /Just do it: simple monadic reasoning/, at http://www.cs.ox.ac.uk/jeremy.gibbons/publications/mr.pdf.
-}



{-# LANGUAGE Rank2Types #-}
module Test.Tasty.QuickCheck.Laws.StateMonad (
    testStateMonadLaws

  -- * State Monad Laws
  , testStateMonadLawPutPut
  , testStateMonadLawPutGet
  , testStateMonadLawGetPut
  , testStateMonadLawGetGet
) where



import Data.Proxy
  ( Proxy(..) )
import Data.Typeable
  ( Typeable, typeRep )
import Test.Tasty
  ( TestTree, testGroup )
import Test.Tasty.QuickCheck
  ( testProperty, Property, Arbitrary(..), CoArbitrary(..) )
import Text.Show.Functions
  ()

import Test.Tasty.QuickCheck.Laws.Class



testStateMonadLaws
  :: ( Monad m
     , Eq s, Eq a
     , Show t, Show s
     , Arbitrary t, Arbitrary s
     , Arbitrary (m a)
     , CoArbitrary s
     , Typeable m, Typeable s, Typeable a
     )
  => Proxy m -> Proxy t -> Proxy s -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> m s -> (s -> m ())
  -> TestTree
testStateMonadLaws pm pt ps pa eq get put =
  let
    label = "State Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "s :: " ++ (show $ typeRep ps) ++ ", " ++
      "a :: " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testStateMonadLawPutPut pm pt ps eq put
      , testStateMonadLawPutGet pm pt ps eq get put
      , testStateMonadLawGetPut pm pt ps eq get put
      , testStateMonadLawGetGet pm pt ps pa eq get
      ]



-- | @put s1 >> put s2 === put s2@
testStateMonadLawPutPut
  :: ( Monad m
     , Show t, Show s
     , Arbitrary t, Arbitrary s
     )
  => Proxy m -> Proxy t -> Proxy s
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (s -> m ())
  -> TestTree
testStateMonadLawPutPut pm pt ps eq put =
  testProperty "put s1 >> put s2 === put s2" $
    stateMonadLawPutPut pm pt ps eq put

stateMonadLawPutPut
  :: (Monad m)
  => Proxy m -> Proxy t -> Proxy s
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool)
  -> (s -> m ())
  -> t -> s -> s -> Bool
stateMonadLawPutPut _ _ _ eq put t s1 s2 =
  (eq t) (put s1 >> put s2) (put s2)



-- | @put s >> get === put s >> return s@
testStateMonadLawPutGet
  :: ( Monad m
     , Eq s
     , Show t, Show s
     , Arbitrary t, Arbitrary s
     )
  => Proxy m -> Proxy t -> Proxy s
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> m s -> (s -> m ())
  -> TestTree
testStateMonadLawPutGet pm pt ps eq get put =
  testProperty "put s >> get === put s >> return s" $
    stateMonadLawPutGet pm pt ps eq get put

stateMonadLawPutGet
  :: (Monad m, Eq s)
  => Proxy m -> Proxy t -> Proxy s
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> m s -> (s -> m ())
  -> t -> s -> Bool
stateMonadLawPutGet _ _ _ eq get put t s =
  (eq t) (put s >> get) (put s >> return s)



-- | @get >>= put === return ()@
testStateMonadLawGetPut
  :: ( Monad m
     , Show t
     , Arbitrary t
     )
  => Proxy m -> Proxy t -> Proxy s
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> m s -> (s -> m ())
  -> TestTree
testStateMonadLawGetPut pm pt ps eq get put =
  testProperty "get >>= put === return ()" $
    stateMonadLawGetPut pm pt ps eq get put

stateMonadLawGetPut
  :: (Monad m)
  => Proxy m -> Proxy t -> Proxy s
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> m s -> (s -> m ())
  -> t -> Bool
stateMonadLawGetPut _ _ _ eq get put t =
  (eq t) (get >>= put) (return ())



-- | @get >>= \\s -> get >>= k s === get >>= \\s -> k s s@
testStateMonadLawGetGet
  :: ( Monad m
     , Eq a
     , Show t
     , Arbitrary t
     , Arbitrary (m a)
     , CoArbitrary s
     )
  => Proxy m -> Proxy t -> Proxy s -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> m s
  -> TestTree
testStateMonadLawGetGet pm pt ps pa eq get =
  testProperty "get >>= \\s -> get >>= k s === get >>= \\s -> k s s" $
    stateMonadLawGetGet pm pt ps pa eq get

stateMonadLawGetGet
  :: (Monad m, Eq a)
  => Proxy m -> Proxy t -> Proxy s -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> m s
  -> t -> (s -> s -> m a) -> Bool
stateMonadLawGetGet _ _ _ _ eq get t k =
  (eq t) (get >>= \s -> get >>= k s) (get >>= \s -> k s s)
