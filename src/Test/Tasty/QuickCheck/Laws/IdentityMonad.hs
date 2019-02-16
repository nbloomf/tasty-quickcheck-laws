{- |
Module      : Test.Tasty.QuickCheck.Laws.IdentityMonad
Description : Prefab tasty trees of quickcheck properties for the identity monad laws
Copyright   : 2019, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}



{-# LANGUAGE Rank2Types #-}
module Test.Tasty.QuickCheck.Laws.IdentityMonad (
    testIdentityMonadLaws

  -- * Identity Monad Laws
  , testIdentityMonadLawUnwrapReturn
  , testIdentityMonadLawReturnUnwrap
  , testIdentityMonadLawBind
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



-- | Constructs a @TestTree@ checking that the identity monad laws hold for @m@ with value types @a@ and @b@, using a given equality test for values of type @forall u. m u@. The equality context type @t@ is for constructors @m@ from which we can only extract a value within a context, such as reader-like constructors.
testIdentityMonadLaws
  :: ( Monad m
     , Eq a, Eq b
     , Show t, Show a
     , Show (m a)
     , Arbitrary t, Arbitrary a, CoArbitrary a
     , Arbitrary (m a), Arbitrary (m b)
     , Typeable m, Typeable a
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall a. m a -> a) -- ^ @unwrap@
  -> TestTree
testIdentityMonadLaws pm pt pa pb eq unwrap =
  let
    label = "Identity Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "a :: " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testIdentityMonadLawUnwrapReturn pm pt pa unwrap
      , testIdentityMonadLawReturnUnwrap pm pt pa eq unwrap
      , testIdentityMonadLawBind pm pt pa pb eq unwrap
      ]



-- | @unwrap . return === id@
testIdentityMonadLawUnwrapReturn
  :: ( Monad m, Eq a, Show a
     , Show t
     , Arbitrary t, Arbitrary a, Show (m a)
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy a -- ^ Value type
  -> (forall a. m a -> a) -- ^ @unwrap@
  -> TestTree
testIdentityMonadLawUnwrapReturn pm pt pa unwrap =
  testProperty "unwrap . return === id" $
    identityMonadLawUnwrapReturn pm pt pa unwrap

identityMonadLawUnwrapReturn
  :: (Monad m, Eq a)
  => Proxy m -> Proxy t -> Proxy a
  -> (forall a. m a -> a) -- ^ @unwrap@
  -> t -> a -> Bool
identityMonadLawUnwrapReturn _ _ _ unwrap t x =
  (unwrap . return $ x) == x



-- | @unwrap . return === id@
testIdentityMonadLawReturnUnwrap
  :: ( Monad m, Eq a
     , Show t
     , Arbitrary t, Arbitrary (m a), Show (m a)
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall a. m a -> a) -- ^ @unwrap@
  -> TestTree
testIdentityMonadLawReturnUnwrap pm pt pa eq bail =
  testProperty "return . unwrap == id" $
    identityMonadLawReturnUnwrap pm pt pa eq bail

identityMonadLawReturnUnwrap
  :: (Monad m, Eq a)
  => Proxy m -> Proxy t -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall a. m a -> a) -- ^ unwrap
  -> t -> m a -> Bool
identityMonadLawReturnUnwrap _ _ _ eq unwrap t x =
  (eq t) (return . unwrap $ x) (x)



-- | @unwrap (x >>= f) === unwrap (f (unwrap x))@
testIdentityMonadLawBind
  :: ( Monad m, Eq b
     , Show t, CoArbitrary a, Arbitrary (m b)
     , Arbitrary t, Arbitrary (m a), Show (m a)
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall a. m a -> a) -- ^ @unwrap@
  -> TestTree
testIdentityMonadLawBind pm pt pa pb eq unwrap =
  testProperty "x >>= f === f (unwrap x)" $
    identityMonadLawBind pm pt pa pb eq unwrap

identityMonadLawBind
  :: (Monad m, Eq b)
  => Proxy m -> Proxy t -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall a. m a -> a) -- ^ @unwrap@
  -> t -> m a -> (a -> m b) -> Bool
identityMonadLawBind _ _ _ _ eq unwrap t x f =
  (eq t) (x >>= f) (f (unwrap x))
