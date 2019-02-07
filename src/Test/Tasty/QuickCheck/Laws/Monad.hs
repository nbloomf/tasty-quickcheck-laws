{- |
Module      : Test.Tasty.QuickCheck.Laws.Monad
Description : Prefab tasty trees of quickcheck properties for the Monad laws
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

Prebuilt tasty test trees for the @Monad@ laws. To get started, look at @testMonadLaws@.
-}



{-# LANGUAGE Rank2Types #-}
module Test.Tasty.QuickCheck.Laws.Monad (
     testMonadLaws

  -- * Monad Laws
  ,  testMonadLawRightIdentity
  ,  testMonadLawLeftIdentity
  ,  testMonadLawAssociativity

  -- * Test Trees
  , testMonadLaws1
  , testMonadLaws2
  , testMonadLaws3
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



-- | Constructs a @TestTree@ checking that the @Monad@ class laws hold for @m@ with value types @a@, @b@, and @c@, using a given equality test for values of type @forall u. m u@. The equality context type @t@ is for constructors @m@ from which we can only extract a value within a context, such as reader-like constructors.
testMonadLaws
  :: ( Monad m
     , Eq a, Eq b, Eq c
     , Show a, Show t, Show (m a)
     , Arbitrary a, Arbitrary t, Arbitrary (m a), Arbitrary (m b), Arbitrary (m c)
     , CoArbitrary a, CoArbitrary b
     , Typeable m, Typeable a, Typeable b, Typeable c
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> Proxy c -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadLaws pm pt pa pb pc eq =
  let
    label = "Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "a :: " ++ (show $ typeRep pa) ++ ", " ++
      "b :: " ++ (show $ typeRep pb) ++ ", " ++
      "c :: " ++ (show $ typeRep pc)
  in
    testGroup label
      [ testMonadLawRightIdentity pm pt pa eq
      , testMonadLawLeftIdentity pm pt pa pb eq
      , testMonadLawAssociativity pm pt pa pb pc eq
      ]



-- | @x >>= return === x@
testMonadLawRightIdentity
  :: ( Monad m
     , Eq a
     , Show t
     , Show (m a)
     , Arbitrary t
     , Arbitrary (m a)
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadLawRightIdentity pm pt pa eq =
  testProperty "x >>= return === x" $
    monadLawRightIdentity pm pt pa eq

monadLawRightIdentity
  :: (Monad m, Eq a)
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool)
  -> t -> m a -> Bool
monadLawRightIdentity _ _ _ eq t x =
  (eq t) (x >>= return) (x)



-- | @return a >>= f === f a@
testMonadLawLeftIdentity
  :: ( Monad m
     , Eq b
     , Show a, Show t
     , Arbitrary a, Arbitrary t, Arbitrary (m b)
     , CoArbitrary a
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadLawLeftIdentity pm pt pa pb eq =
  testProperty "return a >>= f === f a" $
    monadLawLeftIdentity pm pt pa pb eq

monadLawLeftIdentity
  :: (Monad m, Eq b)
  => Proxy m -> Proxy t -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool)
  -> t -> a -> (a -> m b) -> Bool
monadLawLeftIdentity _ _ _ _ eq t x f =
  (eq t) ((return x) >>= f) (f x)



-- | @(x >>= f) >>= g === x >>= (\\z -> f z >>= g)@
testMonadLawAssociativity
  :: ( Monad m
     , Eq c
     , Show t, Show (m a)
     , Arbitrary t, Arbitrary (m a), Arbitrary (m b), Arbitrary (m c)
     , CoArbitrary a, CoArbitrary b
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> Proxy c -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadLawAssociativity pm pt pa pb pc eq =
  testProperty "(x >>= f) >>= g === x >>= (\\z -> f z >>= g)" $
    monadLawAssociativity pm pt pa pb pc eq

monadLawAssociativity
  :: ( Monad m
     , Eq c
     )
  => Proxy m -> Proxy t -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool)
  -> t -> m a -> (a -> m b) -> (b -> m c) -> Bool
monadLawAssociativity _ _ _ _ _ eq t x f g =
  (eq t) ((x >>= f) >>= g) (x >>= (\z -> f z >>= g))





-- | All possible value type selections for @testMonadLaws@ from one choice
testMonadLaws1
  :: ( Monad m
     , Checkable a
     , Show t, Show (m a)
     , Arbitrary t, Arbitrary (m a)
     , Typeable m
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadLaws1 pm pt pa eq =
  let label = "Monad Laws for " ++ (show $ typeRep pm) in
  testGroup label
    [ testMonadLaws pm pt pa pa pa eq
    ]



-- | All possible value type selections for @testMonadLaws@ from two choices
testMonadLaws2
  :: ( Monad m
     , Checkable a, Checkable b
     , Show t, Show (m a), Show (m b)
     , Arbitrary t, Arbitrary (m a), Arbitrary (m b)
     , Typeable m
     )
  => Proxy m -> Proxy t -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadLaws2 pm pt pa pb eq =
  let label = "Monad Laws for " ++ (show $ typeRep pm) in
  testGroup label
    [ testMonadLaws pm pt pa pa pa eq
    , testMonadLaws pm pt pa pa pb eq
    , testMonadLaws pm pt pa pb pa eq
    , testMonadLaws pm pt pa pb pb eq
    , testMonadLaws pm pt pb pa pa eq
    , testMonadLaws pm pt pb pa pb eq
    , testMonadLaws pm pt pb pb pa eq
    , testMonadLaws pm pt pb pb pb eq
    ]



-- | All possible value type selections for @testMonadLaws@ from three choices
testMonadLaws3
  :: ( Monad m
     , Checkable a, Checkable b, Checkable c
     , Show t, Show (m a), Show (m b), Show (m c)
     , Arbitrary t, Arbitrary (m a), Arbitrary (m b), Arbitrary (m c)
     , Typeable m
     )
  => Proxy m -> Proxy t -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadLaws3 pm pt pa pb pc eq =
  let label = "Monad Laws for " ++ (show $ typeRep pm) in
  testGroup label
    [ testMonadLaws pm pt pa pa pa eq
    , testMonadLaws pm pt pa pa pb eq
    , testMonadLaws pm pt pa pa pc eq
    , testMonadLaws pm pt pa pb pa eq
    , testMonadLaws pm pt pa pb pb eq
    , testMonadLaws pm pt pa pb pc eq
    , testMonadLaws pm pt pa pc pa eq
    , testMonadLaws pm pt pa pc pb eq
    , testMonadLaws pm pt pa pc pc eq
    , testMonadLaws pm pt pb pa pa eq
    , testMonadLaws pm pt pb pa pb eq
    , testMonadLaws pm pt pb pa pc eq
    , testMonadLaws pm pt pb pb pa eq
    , testMonadLaws pm pt pb pb pb eq
    , testMonadLaws pm pt pb pb pc eq
    , testMonadLaws pm pt pb pc pa eq
    , testMonadLaws pm pt pb pc pb eq
    , testMonadLaws pm pt pb pc pc eq
    , testMonadLaws pm pt pc pa pa eq
    , testMonadLaws pm pt pc pa pb eq
    , testMonadLaws pm pt pc pa pc eq
    , testMonadLaws pm pt pc pb pa eq
    , testMonadLaws pm pt pc pb pb eq
    , testMonadLaws pm pt pc pb pc eq
    , testMonadLaws pm pt pc pc pa eq
    , testMonadLaws pm pt pc pc pb eq
    , testMonadLaws pm pt pc pc pc eq
    ]
