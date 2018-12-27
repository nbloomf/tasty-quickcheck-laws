{- |
Module      : Test.Tasty.QuickCheck.Laws.Applicative
Description : Prefab tasty trees of quickcheck properties for the Applicative laws
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

Prebuilt tasty test trees for the @Applicative@ functor laws. To get started, look at @testApplicativeLaws@.
-}



{-# LANGUAGE Rank2Types #-}
module Test.Tasty.QuickCheck.Laws.Applicative (
    testApplicativeLaws

  -- * Applicative Laws
  , testApplicativeLawIdentity
  , testApplicativeLawHomomorphism
  , testApplicativeLawInterchange
  , testApplicativeLawComposite

  -- * Test Trees
  , testApplicativeLaws1
  , testApplicativeLaws2
  , testApplicativeLaws3
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



-- | Constructs a @TestTree@ checking that the four @Applicative@ class laws hold for @f@ with value types @a@, @b@, and @c@, using a given equality test for values of type @forall u. f u@. The equality context type @t@ is for constructors @f@ from which we can only extract a value within a context, such as reader-like constructors.
testApplicativeLaws
  :: ( Applicative f
     , Eq a, Eq b, Eq c
     , Show a, Show t
     , Show (f a), Show (f (a -> b)), Show (f (b -> c))
     , Arbitrary a, Arbitrary b, Arbitrary t
     , Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c))
     , CoArbitrary a
     , Typeable f, Typeable a, Typeable b, Typeable c
     )
  => Proxy f -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @f@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> Proxy c -- ^ Value type
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLaws pf pt pa pb pc eq =
  let
    label = "Applicative Laws for " ++ (show $ typeRep pf) ++ " with " ++
      "a :: " ++ (show $ typeRep pa) ++ ", " ++
      "b :: " ++ (show $ typeRep pb) ++ ", " ++
      "c :: " ++ (show $ typeRep pc)
  in
    testGroup label
      [ testApplicativeLawIdentity pf pt pa eq
      , testApplicativeLawHomomorphism pf pt pa pb eq
      , testApplicativeLawInterchange pf pt pa pb eq
      , testApplicativeLawComposite pf pt pa pb pc eq
      ]



-- | @pure id \<*> x === x@
testApplicativeLawIdentity
  :: ( Applicative f
     , Eq a
     , Show (f a), Show t
     , Arbitrary (f a), Arbitrary t
     )
  => Proxy f -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @f@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLawIdentity pf pt pa eq =
  testProperty "pure id <*> x === x" $
    applicativeLawIdentity pf pt pa eq

applicativeLawIdentity
  :: (Applicative f, Eq a)
  => Proxy f -> Proxy t -> Proxy a
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> t -> f a -> Bool
applicativeLawIdentity _ _ _ eq t x =
  (eq t) (pure id <*> x) x



-- | @pure f \<*> pure a === pure (f a)@
testApplicativeLawHomomorphism
  :: ( Applicative f
     , Eq b
     , Show a, Show t
     , Arbitrary a, Arbitrary b, Arbitrary t
     , CoArbitrary a
     )
  => Proxy f -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @f@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLawHomomorphism pf pt pa pb eq =
  testProperty "pure f <*> pure a === pure (f a)" $
    applicativeLawHomomorphism pf pt pa pb eq

applicativeLawHomomorphism
  :: (Applicative f, Eq b)
  => Proxy f -> Proxy t -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> t -> (a -> b) -> a -> Bool
applicativeLawHomomorphism _ _ _ _ eq t f a =
  (eq t) (pure f <*> pure a) (pure (f a))



-- | @x \<*> pure a === pure ($ a) \<*> x@
testApplicativeLawInterchange
  :: ( Applicative f
     , Eq b
     , Show a, Show t
     , Show (f (a -> b))
     , Arbitrary a, Arbitrary t
     , Arbitrary (f (a -> b))
     )
  => Proxy f -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @f@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLawInterchange pf pt pa pb eq =
  testProperty "x <*> pure a === pure ($ a) <*> x" $
    applicativeLawInterchange pf pt pa pb eq

applicativeLawInterchange
  :: (Applicative f, Eq b)
  => Proxy f -> Proxy t -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> t -> f (a -> b) -> a -> Bool
applicativeLawInterchange _ _ _ _ eq t x a =
  (eq t) (x <*> pure a) (pure ($ a) <*> x)



-- | @pure (.) \<*> x \<*> y \<*> z = x \<*> (y \<*> z)@
testApplicativeLawComposite
  :: ( Applicative f
     , Eq c
     , Show t, Show (f a), Show (f (b -> c)), Show (f (a -> b))
     , Arbitrary t, Arbitrary (f a), Arbitrary (f (b -> c)), Arbitrary (f (a -> b))
     )
  => Proxy f -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @f@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> Proxy c -- ^ Value type
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLawComposite pf pt pa pb pc eq =
  testProperty "pure (.) <*> x <*> y <*> z = x <*> (y <*> z)" $
    applicativeLawComposite pf pt pa pb pc eq

applicativeLawComposite
  :: (Applicative f, Eq c)
  => Proxy f -> Proxy t -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool)
  -> t -> f (b -> c) -> f (a -> b) -> f a -> Bool
applicativeLawComposite _ _ _ _ _ eq t x y z =
  (eq t) (pure (.) <*> x <*> y <*> z) (x <*> (y <*> z))





-- | All possible value type selections for @testApplicativeLaws@ from one choice
testApplicativeLaws1
  :: ( Applicative f
     , Checkable a
     , Show (f a), Show t
     , Show (f (a -> a))
     , Arbitrary (f a), Arbitrary t
     , Arbitrary (f (a -> a))
     , Typeable f
     )
  => Proxy f -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context type for @f@
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLaws1 pf pt pa eq =
  let label = "Applicative Laws for " ++ (show $ typeRep pf) in
  testGroup label
    [ testApplicativeLaws pf pt pa pa pa eq
    ]



-- | All possible value type selections for @testApplicativeLaws@ from two choices
testApplicativeLaws2
  :: ( Applicative f
     , Checkable a, Checkable b
     , Show (f a), Show (f b), Show t
     , Show (f (a -> a)), Show (f (a -> b))
     , Show (f (b -> a)), Show (f (b -> b))
     , Arbitrary (f a), Arbitrary (f b), Arbitrary t
     , Arbitrary (f (a -> a)), Arbitrary (f (a -> b))
     , Arbitrary (f (b -> a)), Arbitrary (f (b -> b))
     , Typeable f
     )
  => Proxy f -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @f@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLaws2 pf pt pa pb eq =
  let label = "Applicative Laws for " ++ (show $ typeRep pf) in
  testGroup label
    [ testApplicativeLaws pf pt pa pa pa eq
    , testApplicativeLaws pf pt pa pa pb eq
    , testApplicativeLaws pf pt pa pb pa eq
    , testApplicativeLaws pf pt pa pb pb eq
    , testApplicativeLaws pf pt pb pa pa eq
    , testApplicativeLaws pf pt pb pa pb eq
    , testApplicativeLaws pf pt pb pb pa eq
    , testApplicativeLaws pf pt pb pb pb eq
    ]



-- | All possible value type selections for @testApplicativeLaws@ from three choices
testApplicativeLaws3
  :: ( Applicative f
     , Checkable a, Checkable b, Checkable c
     , Show (f a), Show (f b), Show (f c), Show t
     , Show (f (a -> a)), Show (f (a -> b)), Show (f (a -> c))
     , Show (f (b -> a)), Show (f (b -> b)), Show (f (b -> c))
     , Show (f (c -> a)), Show (f (c -> b)), Show (f (c -> c))
     , Arbitrary (f a), Arbitrary (f b), Arbitrary (f c), Arbitrary t
     , Arbitrary (f (a -> a)), Arbitrary (f (a -> b)), Arbitrary (f (a -> c))
     , Arbitrary (f (b -> a)), Arbitrary (f (b -> b)), Arbitrary (f (b -> c))
     , Arbitrary (f (c -> a)), Arbitrary (f (c -> b)), Arbitrary (f (c -> c))
     , Typeable f
     )
  => Proxy f -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @f@
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> Proxy c -- ^ Value type
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLaws3 pf pt pa pb pc eq =
  let label = "Applicative Laws for " ++ (show $ typeRep pf) in
  testGroup label
    [ testApplicativeLaws pf pt pa pa pa eq
    , testApplicativeLaws pf pt pa pa pb eq
    , testApplicativeLaws pf pt pa pa pc eq
    , testApplicativeLaws pf pt pa pb pa eq
    , testApplicativeLaws pf pt pa pb pb eq
    , testApplicativeLaws pf pt pa pb pc eq
    , testApplicativeLaws pf pt pa pc pa eq
    , testApplicativeLaws pf pt pa pc pb eq
    , testApplicativeLaws pf pt pa pc pc eq
    , testApplicativeLaws pf pt pb pa pa eq
    , testApplicativeLaws pf pt pb pa pb eq
    , testApplicativeLaws pf pt pb pa pc eq
    , testApplicativeLaws pf pt pb pb pa eq
    , testApplicativeLaws pf pt pb pb pb eq
    , testApplicativeLaws pf pt pb pb pc eq
    , testApplicativeLaws pf pt pb pc pa eq
    , testApplicativeLaws pf pt pb pc pb eq
    , testApplicativeLaws pf pt pb pc pc eq
    , testApplicativeLaws pf pt pc pa pa eq
    , testApplicativeLaws pf pt pc pa pb eq
    , testApplicativeLaws pf pt pc pa pc eq
    , testApplicativeLaws pf pt pc pb pa eq
    , testApplicativeLaws pf pt pc pb pb eq
    , testApplicativeLaws pf pt pc pb pc eq
    , testApplicativeLaws pf pt pc pc pa eq
    , testApplicativeLaws pf pt pc pc pb eq
    , testApplicativeLaws pf pt pc pc pc eq
    ]
