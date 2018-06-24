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
     , Show (f a)
     , Arbitrary b, Arbitrary c
     , Arbitrary (f a)
     , CoArbitrary a, CoArbitrary b
     , Typeable f, Typeable a, Typeable b, Typeable c
     )
  => Proxy f -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testFunctorLaws pf pa pb pc eq =
  let
    label = "Functor Laws for " ++ (show $ typeRep pf) ++ " with " ++
      "a :: " ++ (show $ typeRep pa) ++ ", " ++
      "b :: " ++ (show $ typeRep pb) ++ ", " ++
      "c :: " ++ (show $ typeRep pc)
  in
    testGroup label
      [ testFunctorLawIdentity pf pa eq
      , testFunctorLawComposite pf pa pb pc eq
      ]



-- | @fmap id x === x@
testFunctorLawIdentity
  :: ( Functor f
     , Eq a
     , Show (f a)
     , Arbitrary (f a)
     )
  => Proxy f -> Proxy a
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testFunctorLawIdentity pf pa eq =
  testProperty "fmap id x === x" $
    functorLawIdentity pf pa eq

functorLawIdentity
  :: (Functor f, Eq a)
  => Proxy f -> Proxy a
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> f a -> Bool
functorLawIdentity _ _ eq x =
  eq (fmap id x) x



-- | @fmap (f . g) x === (fmap f . fmap g) x@
testFunctorLawComposite
  :: ( Functor f
     , Eq c
     , Show (f a)
     , Arbitrary b, Arbitrary c
     , Arbitrary (f a)
     , CoArbitrary a, CoArbitrary b
     )
  => Proxy f -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testFunctorLawComposite pf pa pb pc eq =
  testProperty "fmap (f . g) x === (fmap f . fmap g) x" $
    functorLawComposite pf pa pb pc eq

functorLawComposite
  :: (Functor f, Eq c)
  => Proxy f -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => f u -> f u -> Bool)
  -> (f a) -> (b -> c) -> (a -> b) -> Bool
functorLawComposite _ _ _ _ eq x f g =
  eq (fmap (f . g) x) ((fmap f . fmap g) x)





-- | All possible selections from 1 type
testFunctorLaws1
  :: ( Functor f
     , Checkable a
     , Show (f a)
     , Arbitrary (f a)
     , Typeable f
     )
  => Proxy f -> Proxy a
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testFunctorLaws1 pf pa eq =
  let label = "Functor Laws for " ++ (show $ typeRep pf) in
  testGroup label
    [ testFunctorLaws pf pa pa pa eq
    ]



-- | All possible selections from 2 types
testFunctorLaws2
  :: ( Functor f
     , Checkable a, Checkable b
     , Show (f a), Show (f b)
     , Arbitrary (f a), Arbitrary (f b)
     , Typeable f
     )
  => Proxy f -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testFunctorLaws2 pf pa pb eq =
  let label = "Functor Laws for " ++ (show $ typeRep pf) in
  testGroup label
    [ testFunctorLaws pf pa pa pa eq
    , testFunctorLaws pf pa pa pb eq
    , testFunctorLaws pf pa pb pa eq
    , testFunctorLaws pf pa pb pb eq
    , testFunctorLaws pf pb pa pa eq
    , testFunctorLaws pf pb pa pb eq
    , testFunctorLaws pf pb pb pa eq
    , testFunctorLaws pf pb pb pb eq
    ]



-- | All possible selections from 3 types
testFunctorLaws3
  :: ( Functor f
     , Checkable a, Checkable b, Checkable c
     , Show (f a), Show (f b), Show (f c)
     , Arbitrary (f a), Arbitrary (f b), Arbitrary (f c)
     , Typeable f
     )
  => Proxy f -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testFunctorLaws3 pf pa pb pc eq =
  let label = "Functor Laws for " ++ (show $ typeRep pf) in
  testGroup label
    [ testFunctorLaws pf pa pa pa eq
    , testFunctorLaws pf pa pa pb eq
    , testFunctorLaws pf pa pa pc eq
    , testFunctorLaws pf pa pb pa eq
    , testFunctorLaws pf pa pb pb eq
    , testFunctorLaws pf pa pb pc eq
    , testFunctorLaws pf pa pc pa eq
    , testFunctorLaws pf pa pc pb eq
    , testFunctorLaws pf pa pc pc eq
    , testFunctorLaws pf pb pa pa eq
    , testFunctorLaws pf pb pa pb eq
    , testFunctorLaws pf pb pa pc eq
    , testFunctorLaws pf pb pb pa eq
    , testFunctorLaws pf pb pb pb eq
    , testFunctorLaws pf pb pb pc eq
    , testFunctorLaws pf pb pc pa eq
    , testFunctorLaws pf pb pc pb eq
    , testFunctorLaws pf pb pc pc eq
    , testFunctorLaws pf pc pa pa eq
    , testFunctorLaws pf pc pa pb eq
    , testFunctorLaws pf pc pa pc eq
    , testFunctorLaws pf pc pb pa eq
    , testFunctorLaws pf pc pb pb eq
    , testFunctorLaws pf pc pb pc eq
    , testFunctorLaws pf pc pc pa eq
    , testFunctorLaws pf pc pc pb eq
    , testFunctorLaws pf pc pc pc eq
    ]
