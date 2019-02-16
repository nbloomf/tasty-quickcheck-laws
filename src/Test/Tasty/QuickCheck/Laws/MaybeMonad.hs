{- |
Module      : Test.Tasty.QuickCheck.Laws.MaybeMonad
Description : Prefab tasty trees of quickcheck properties for the maybe monad laws
Copyright   : 2019, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}



{-# LANGUAGE Rank2Types #-}
module Test.Tasty.QuickCheck.Laws.MaybeMonad (
    testMaybeMonadLaws

  -- * Maybe Monad Laws
  , testMaybeMonadLawBailThen
) where

import Data.Proxy
  ( Proxy(..) )
import Data.Typeable
  ( Typeable, typeRep )
import Test.Tasty
  ( TestTree, testGroup )
import Test.Tasty.QuickCheck
  ( testProperty, Property, Arbitrary(..) )
import Text.Show.Functions
  ()

import Test.Tasty.QuickCheck.Laws.Class



-- | Constructs a @TestTree@ checking that the maybe monad laws hold for @m@ with value types @a@ and @b@, using a given equality test for values of type @forall u. m u@. The equality context type @t@ is for constructors @m@ from which we can only extract a value within a context, such as reader-like constructors.
testMaybeMonadLaws
  :: ( Monad m
     , Eq a
     , Show t, Show a
     , Show (m a)
     , Arbitrary t, Arbitrary a
     , Arbitrary (m a)
     , Typeable m, Typeable a
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall a. m a) -- ^ @bail@
  -> TestTree
testMaybeMonadLaws pm pt pa eq bail =
  let
    label = "Maybe Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "a :: " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testMaybeMonadLawBailThen pm pt pa eq bail
      ]



-- | @bail >> x === bail@
testMaybeMonadLawBailThen
  :: ( Monad m, Eq a
     , Show t
     , Arbitrary t, Arbitrary (m a), Show (m a)
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall a. m a) -- ^ @bail@
  -> TestTree
testMaybeMonadLawBailThen pm pt pa eq bail =
  testProperty "bail >> x === x" $
    maybeMonadLawBailThen pm pt pa eq bail

maybeMonadLawBailThen
  :: (Monad m, Eq a)
  => Proxy m -> Proxy t -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall a. m a) -- ^ @bail@
  -> t -> m a -> Bool
maybeMonadLawBailThen _ _ _ eq bail t x =
  (eq t) (bail >> x) (bail)
