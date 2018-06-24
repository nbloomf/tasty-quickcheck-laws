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



testMonadLaws
  :: ( Monad m
     , Eq a, Eq b, Eq c
     , Show a, Show (m a)
     , Arbitrary a, Arbitrary (m a), Arbitrary (m b), Arbitrary (m c)
     , CoArbitrary a, CoArbitrary b
     , Typeable m, Typeable a, Typeable b, Typeable c
     )
  => Proxy m -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadLaws pm pa pb pc eq =
  let
    label = "Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "a :: " ++ (show $ typeRep pa) ++ ", " ++
      "b :: " ++ (show $ typeRep pb) ++ ", " ++
      "c :: " ++ (show $ typeRep pc)
  in
    testGroup label
      [  testMonadLawRightIdentity pm pa eq
      ,  testMonadLawLeftIdentity pm pa pb eq
      ,  testMonadLawAssociativity pm pa pb pc eq
      ]



-- | @x >>= return === x@
testMonadLawRightIdentity
  :: ( Monad m
     , Eq a
     , Show (m a)
     , Arbitrary (m a)
     )
  => Proxy m -> Proxy a
  -> (forall u. (Eq u) => m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadLawRightIdentity pm pa eq =
  testProperty "x >>= return === x" $
    monadLawRightIdentity pm pa eq

monadLawRightIdentity
  :: (Monad m, Eq a)
  => Proxy m -> Proxy a
  -> (forall u. (Eq u) => m u -> m u -> Bool)
  -> m a -> Bool
monadLawRightIdentity _ _ eq x =
  eq (x >>= return) (x)



-- | @return a >>= f === f a@
testMonadLawLeftIdentity
  :: ( Monad m
     , Eq b
     , Show a
     , Arbitrary a, Arbitrary (m b)
     , CoArbitrary a
     )
  => Proxy m -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadLawLeftIdentity pm pa pb eq =
  testProperty "return a >>= f === f a" $
    monadLawLeftIdentity pm pa pb eq

monadLawLeftIdentity
  :: (Monad m, Eq b)
  => Proxy m -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => m u -> m u -> Bool)
  -> a -> (a -> m b) -> Bool
monadLawLeftIdentity _ _ _ eq x f =
  eq ((return x) >>= f) (f x)



-- | @(x >>= f) >>= g === x >>= (\\z -> f z >>= g)@
testMonadLawAssociativity
  :: ( Monad m
     , Eq c
     , Show (m a)
     , Arbitrary (m a), Arbitrary (m b), Arbitrary (m c)
     , CoArbitrary a, CoArbitrary b
     )
  => Proxy m -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadLawAssociativity pm pa pb pc eq =
  testProperty "(x >>= f) >>= g === x >>= (\\z -> f z >>= g)" $
    monadLawAssociativity pm pa pb pc eq

monadLawAssociativity
  :: ( Monad m
     , Eq c
     )
  => Proxy m -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => m u -> m u -> Bool)
  -> m a -> (a -> m b) -> (b -> m c) -> Bool
monadLawAssociativity _ _ _ _ eq x f g =
  eq ((x >>= f) >>= g) (x >>= (\z -> f z >>= g))





-- | All possible selections from 1 type
testMonadLaws1
  :: ( Monad m
     , Checkable a
     , Show (m a)
     , Arbitrary (m a)
     , Typeable m
     )
  => Proxy m -> Proxy a
  -> (forall u. (Eq u) => m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadLaws1 pm pa eq =
  let label = "Monad Laws for " ++ (show $ typeRep pm) in
  testGroup label
    [ testMonadLaws pm pa pa pa eq
    ]



-- | All possible selections from 2 types
testMonadLaws2
  :: ( Monad m
     , Checkable a, Checkable b
     , Show (m a), Show (m b)
     , Arbitrary (m a), Arbitrary (m b)
     , Typeable m
     )
  => Proxy m -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadLaws2 pm pa pb eq =
  let label = "Monad Laws for " ++ (show $ typeRep pm) in
  testGroup label
    [ testMonadLaws pm pa pa pa eq
    , testMonadLaws pm pa pa pb eq
    , testMonadLaws pm pa pb pa eq
    , testMonadLaws pm pa pb pb eq
    , testMonadLaws pm pb pa pa eq
    , testMonadLaws pm pb pa pb eq
    , testMonadLaws pm pb pb pa eq
    , testMonadLaws pm pb pb pb eq
    ]



-- | All possible selections from 3 types
testMonadLaws3
  :: ( Monad m
     , Checkable a, Checkable b, Checkable c
     , Show (m a), Show (m b), Show (m c)
     , Arbitrary (m a), Arbitrary (m b), Arbitrary (m c)
     , Typeable m
     )
  => Proxy m -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
testMonadLaws3 pm pa pb pc eq =
  let label = "Monad Laws for " ++ (show $ typeRep pm) in
  testGroup label
    [ testMonadLaws pm pa pa pa eq
    , testMonadLaws pm pa pa pb eq
    , testMonadLaws pm pa pa pc eq
    , testMonadLaws pm pa pb pa eq
    , testMonadLaws pm pa pb pb eq
    , testMonadLaws pm pa pb pc eq
    , testMonadLaws pm pa pc pa eq
    , testMonadLaws pm pa pc pb eq
    , testMonadLaws pm pa pc pc eq
    , testMonadLaws pm pb pa pa eq
    , testMonadLaws pm pb pa pb eq
    , testMonadLaws pm pb pa pc eq
    , testMonadLaws pm pb pb pa eq
    , testMonadLaws pm pb pb pb eq
    , testMonadLaws pm pb pb pc eq
    , testMonadLaws pm pb pc pa eq
    , testMonadLaws pm pb pc pb eq
    , testMonadLaws pm pb pc pc eq
    , testMonadLaws pm pc pa pa eq
    , testMonadLaws pm pc pa pb eq
    , testMonadLaws pm pc pa pc eq
    , testMonadLaws pm pc pb pa eq
    , testMonadLaws pm pc pb pb eq
    , testMonadLaws pm pc pb pc eq
    , testMonadLaws pm pc pc pa eq
    , testMonadLaws pm pc pc pb eq
    , testMonadLaws pm pc pc pc eq
    ]
