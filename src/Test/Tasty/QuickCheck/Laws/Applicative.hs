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



testApplicativeLaws
  :: ( Applicative f
     , Eq a, Eq b, Eq c
     , Show a
     , Show (f a), Show (f (a -> b)), Show (f (b -> c))
     , Arbitrary a, Arbitrary b
     , Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c))
     , CoArbitrary a
     , Typeable f, Typeable a, Typeable b, Typeable c
     )
  => Proxy f -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLaws pf pa pb pc eq =
  let
    label = "Applicative Laws for " ++ (show $ typeRep pf) ++ " with " ++
      "a :: " ++ (show $ typeRep pa) ++ ", " ++
      "b :: " ++ (show $ typeRep pb) ++ ", " ++
      "c :: " ++ (show $ typeRep pc)
  in
    testGroup label
      [ testApplicativeLawIdentity pf pa eq
      , testApplicativeLawHomomorphism pf pa pb eq
      , testApplicativeLawInterchange pf pa pb eq
      , testApplicativeLawComposite pf pa pb pc eq
      ]



-- | @pure id <*> x === x@
testApplicativeLawIdentity
  :: ( Applicative f
     , Eq a
     , Show (f a)
     , Arbitrary (f a)
     )
  => Proxy f -> Proxy a
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLawIdentity pf pa eq =
  testProperty "pure id <*> x === x" $
    applicativeLawIdentity pf pa eq

applicativeLawIdentity
  :: (Applicative f, Eq a)
  => Proxy f -> Proxy a
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> f a -> Bool
applicativeLawIdentity _ _ eq x =
  eq (pure id <*> x) x



-- | @pure f <*> pure a === pure (f a)@
testApplicativeLawHomomorphism
  :: ( Applicative f
     , Eq b
     , Show a
     , Arbitrary a, Arbitrary b
     , CoArbitrary a
     )
  => Proxy f -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLawHomomorphism pf pa pb eq =
  testProperty "pure f <*> pure a === pure (f a)" $
    applicativeLawHomomorphism pf pa pb eq

applicativeLawHomomorphism
  :: (Applicative f, Eq b)
  => Proxy f -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> (a -> b) -> a -> Bool
applicativeLawHomomorphism _ _ _ eq f a =
  eq (pure f <*> pure a) (pure (f a))



-- | @x <*> pure a === pure ($ a) <*> x@
testApplicativeLawInterchange
  :: ( Applicative f
     , Eq b
     , Show a
     , Show (f (a -> b))
     , Arbitrary a
     , Arbitrary (f (a -> b))
     )
  => Proxy f -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLawInterchange pf pa pb eq =
  testProperty "x <*> pure a === pure ($ a) <*> x" $
    applicativeLawInterchange pf pa pb eq

applicativeLawInterchange
  :: (Applicative f, Eq b)
  => Proxy f -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> f (a -> b) -> a -> Bool
applicativeLawInterchange _ _ _ eq x a =
  eq (x <*> pure a) (pure ($ a) <*> x)



-- | @pure (.) <*> x <*> y <*> z = x <*> (y <*> z)@
testApplicativeLawComposite
  :: ( Applicative f
     , Eq c
     , Show (f a), Show (f (b -> c)), Show (f (a -> b))
     , Arbitrary (f a), Arbitrary (f (b -> c)), Arbitrary (f (a -> b))
     )
  => Proxy f -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLawComposite pf pa pb pc eq =
  testProperty "pure (.) <*> x <*> y <*> z = x <*> (y <*> z)" $
    applicativeLawComposite pf pa pb pc eq

applicativeLawComposite
  :: (Applicative f, Eq c)
  => Proxy f -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => f u -> f u -> Bool)
  -> f (b -> c) -> f (a -> b) -> f a -> Bool
applicativeLawComposite _ _ _ _ eq x y z =
  eq (pure (.) <*> x <*> y <*> z) (x <*> (y <*> z))





-- | All possible selections from 1 type
testApplicativeLaws1
  :: ( Applicative f
     , Checkable a
     , Show (f a)
     , Show (f (a -> a))
     , Arbitrary (f a)
     , Arbitrary (f (a -> a))
     , Typeable f
     )
  => Proxy f -> Proxy a
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLaws1 pf pa eq =
  let label = "Functor Laws for " ++ (show $ typeRep pf) in
  testGroup label
    [ testApplicativeLaws pf pa pa pa eq
    ]



-- | All possible selections from 2 types
testApplicativeLaws2
  :: ( Applicative f
     , Checkable a, Checkable b
     , Show (f a), Show (f b)
     , Show (f (a -> a)), Show (f (a -> b))
     , Show (f (b -> a)), Show (f (b -> b))
     , Arbitrary (f a), Arbitrary (f b)
     , Arbitrary (f (a -> a)), Arbitrary (f (a -> b))
     , Arbitrary (f (b -> a)), Arbitrary (f (b -> b))
     , Typeable f
     )
  => Proxy f -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLaws2 pf pa pb eq =
  let label = "Functor Laws for " ++ (show $ typeRep pf) in
  testGroup label
    [ testApplicativeLaws pf pa pa pa eq
    , testApplicativeLaws pf pa pa pb eq
    , testApplicativeLaws pf pa pb pa eq
    , testApplicativeLaws pf pa pb pb eq
    , testApplicativeLaws pf pb pa pa eq
    , testApplicativeLaws pf pb pa pb eq
    , testApplicativeLaws pf pb pb pa eq
    , testApplicativeLaws pf pb pb pb eq
    ]



-- | All possible selections from 3 types
testApplicativeLaws3
  :: ( Applicative f
     , Checkable a, Checkable b, Checkable c
     , Show (f a), Show (f b), Show (f c)
     , Show (f (a -> a)), Show (f (a -> b)), Show (f (a -> c))
     , Show (f (b -> a)), Show (f (b -> b)), Show (f (b -> c))
     , Show (f (c -> a)), Show (f (c -> b)), Show (f (c -> c))
     , Arbitrary (f a), Arbitrary (f b), Arbitrary (f c)
     , Arbitrary (f (a -> a)), Arbitrary (f (a -> b)), Arbitrary (f (a -> c))
     , Arbitrary (f (b -> a)), Arbitrary (f (b -> b)), Arbitrary (f (b -> c))
     , Arbitrary (f (c -> a)), Arbitrary (f (c -> b)), Arbitrary (f (c -> c))
     , Typeable f
     )
  => Proxy f -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => f u -> f u -> Bool) -- ^ Equality test
  -> TestTree
testApplicativeLaws3 pf pa pb pc eq =
  let label = "Functor Laws for " ++ (show $ typeRep pf) in
  testGroup label
    [ testApplicativeLaws pf pa pa pa eq
    , testApplicativeLaws pf pa pa pb eq
    , testApplicativeLaws pf pa pa pc eq
    , testApplicativeLaws pf pa pb pa eq
    , testApplicativeLaws pf pa pb pb eq
    , testApplicativeLaws pf pa pb pc eq
    , testApplicativeLaws pf pa pc pa eq
    , testApplicativeLaws pf pa pc pb eq
    , testApplicativeLaws pf pa pc pc eq
    , testApplicativeLaws pf pb pa pa eq
    , testApplicativeLaws pf pb pa pb eq
    , testApplicativeLaws pf pb pa pc eq
    , testApplicativeLaws pf pb pb pa eq
    , testApplicativeLaws pf pb pb pb eq
    , testApplicativeLaws pf pb pb pc eq
    , testApplicativeLaws pf pb pc pa eq
    , testApplicativeLaws pf pb pc pb eq
    , testApplicativeLaws pf pb pc pc eq
    , testApplicativeLaws pf pc pa pa eq
    , testApplicativeLaws pf pc pa pb eq
    , testApplicativeLaws pf pc pa pc eq
    , testApplicativeLaws pf pc pb pa eq
    , testApplicativeLaws pf pc pb pb eq
    , testApplicativeLaws pf pc pb pc eq
    , testApplicativeLaws pf pc pc pa eq
    , testApplicativeLaws pf pc pc pb eq
    , testApplicativeLaws pf pc pc pc eq
    ]

