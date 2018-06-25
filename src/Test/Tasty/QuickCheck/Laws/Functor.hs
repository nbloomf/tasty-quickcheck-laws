{-# LANGUAGE Rank2Types #-}
module Test.Tasty.QuickCheck.Laws.Functor (
    testFunctorLaws

  -- * Functor Laws
  , testFunctorLawIdentity
  , testFunctorLawComposite

  -- * Test Trees
  , testFunctorLaws1
  , testFunctorLaws2
  , testFunctorLaws3
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



testFunctorLaws
  :: ( Functor f
     , Eq a, Eq c
     , Show t, Show (f a)
     , Arbitrary t, Arbitrary b, Arbitrary c
     , Arbitrary (f a)
     , CoArbitrary a, CoArbitrary b
     , Typeable f, Typeable a, Typeable b, Typeable c
     )
  => Proxy f -> Proxy t -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testFunctorLaws pf pt pa pb pc eq =
  let
    label = "Functor Laws for " ++ (show $ typeRep pf) ++ " with " ++
      "a :: " ++ (show $ typeRep pa) ++ ", " ++
      "b :: " ++ (show $ typeRep pb) ++ ", " ++
      "c :: " ++ (show $ typeRep pc)
  in
    testGroup label
      [ testFunctorLawIdentity pf pt pa eq
      , testFunctorLawComposite pf pt pa pb pc eq
      ]



-- | @fmap id x === x@
testFunctorLawIdentity
  :: ( Functor f
     , Eq a
     , Show t, Show (f a)
     , Arbitrary t, Arbitrary (f a)
     )
  => Proxy f -> Proxy t -> Proxy a
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testFunctorLawIdentity pf pt pa eq =
  testProperty "fmap id x === x" $
    functorLawIdentity pf pt pa eq

functorLawIdentity
  :: (Functor f, Eq a)
  => Proxy f -> Proxy t -> Proxy a
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> t -> f a -> Bool
functorLawIdentity _ _ _ eq t x =
  (eq t) (fmap id x) x



-- | @fmap (f . g) x === (fmap f . fmap g) x@
testFunctorLawComposite
  :: ( Functor f
     , Eq c
     , Show t, Show (f a)
     , Arbitrary t, Arbitrary b, Arbitrary c
     , Arbitrary (f a)
     , CoArbitrary a, CoArbitrary b
     )
  => Proxy f -> Proxy t -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testFunctorLawComposite pf pt pa pb pc eq =
  testProperty "fmap (f . g) x === (fmap f . fmap g) x" $
    functorLawComposite pf pt pa pb pc eq

functorLawComposite
  :: (Functor f, Eq c)
  => Proxy f -> Proxy t -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool)
  -> t -> (f a) -> (b -> c) -> (a -> b) -> Bool
functorLawComposite _ _ _ _ _ eq t x f g =
  (eq t) (fmap (f . g) x) ((fmap f . fmap g) x)





-- | All possible selections from 1 type
testFunctorLaws1
  :: ( Functor f
     , Checkable a
     , Show t, Show (f a)
     , Arbitrary t, Arbitrary (f a)
     , Typeable f
     )
  => Proxy f -> Proxy t -> Proxy a
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testFunctorLaws1 pf pt pa eq =
  let label = "Functor Laws for " ++ (show $ typeRep pf) in
  testGroup label
    [ testFunctorLaws pf pt pa pa pa eq
    ]



-- | All possible selections from 2 types
testFunctorLaws2
  :: ( Functor f
     , Checkable a, Checkable b
     , Show t, Show (f a), Show (f b)
     , Arbitrary t, Arbitrary (f a), Arbitrary (f b)
     , Typeable f
     )
  => Proxy f -> Proxy t -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testFunctorLaws2 pf pt pa pb eq =
  let label = "Functor Laws for " ++ (show $ typeRep pf) in
  testGroup label
    [ testFunctorLaws pf pt pa pa pa eq
    , testFunctorLaws pf pt pa pa pb eq
    , testFunctorLaws pf pt pa pb pa eq
    , testFunctorLaws pf pt pa pb pb eq
    , testFunctorLaws pf pt pb pa pa eq
    , testFunctorLaws pf pt pb pa pb eq
    , testFunctorLaws pf pt pb pb pa eq
    , testFunctorLaws pf pt pb pb pb eq
    ]



-- | All possible selections from 3 types
testFunctorLaws3
  :: ( Functor f
     , Checkable a, Checkable b, Checkable c
     , Show t, Show (f a), Show (f b), Show (f c)
     , Arbitrary t, Arbitrary (f a), Arbitrary (f b), Arbitrary (f c)
     , Typeable f
     )
  => Proxy f -> Proxy t -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => t -> f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testFunctorLaws3 pf pt pa pb pc eq =
  let label = "Functor Laws for " ++ (show $ typeRep pf) in
  testGroup label
    [ testFunctorLaws pf pt pa pa pa eq
    , testFunctorLaws pf pt pa pa pb eq
    , testFunctorLaws pf pt pa pa pc eq
    , testFunctorLaws pf pt pa pb pa eq
    , testFunctorLaws pf pt pa pb pb eq
    , testFunctorLaws pf pt pa pb pc eq
    , testFunctorLaws pf pt pa pc pa eq
    , testFunctorLaws pf pt pa pc pb eq
    , testFunctorLaws pf pt pa pc pc eq
    , testFunctorLaws pf pt pb pa pa eq
    , testFunctorLaws pf pt pb pa pb eq
    , testFunctorLaws pf pt pb pa pc eq
    , testFunctorLaws pf pt pb pb pa eq
    , testFunctorLaws pf pt pb pb pb eq
    , testFunctorLaws pf pt pb pb pc eq
    , testFunctorLaws pf pt pb pc pa eq
    , testFunctorLaws pf pt pb pc pb eq
    , testFunctorLaws pf pt pb pc pc eq
    , testFunctorLaws pf pt pc pa pa eq
    , testFunctorLaws pf pt pc pa pb eq
    , testFunctorLaws pf pt pc pa pc eq
    , testFunctorLaws pf pt pc pb pa eq
    , testFunctorLaws pf pt pc pb pb eq
    , testFunctorLaws pf pt pc pb pc eq
    , testFunctorLaws pf pt pc pc pa eq
    , testFunctorLaws pf pt pc pc pb eq
    , testFunctorLaws pf pt pc pc pc eq
    ]
