{-# LANGUAGE Rank2Types #-}
module Test.Tasty.QuickCheck.Laws.Monad (
    test_monad_laws

  -- * Monad Laws
  , test_monad_law_right_identity
  , test_monad_law_left_identity
  , test_monad_law_associativity

  -- * Test Trees
  , test_monad_laws_1
  , test_monad_laws_2
  , test_monad_laws_3
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



test_monad_laws
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
test_monad_laws pm pa pb pc eq =
  let
    label = "Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "a :: " ++ (show $ typeRep pa) ++ ", " ++
      "b :: " ++ (show $ typeRep pb) ++ ", " ++
      "c :: " ++ (show $ typeRep pc)
  in
    testGroup label
      [ test_monad_law_right_identity pm pa eq
      , test_monad_law_left_identity pm pa pb eq
      , test_monad_law_associativity pm pa pb pc eq
      ]



-- | @x >>= return === x@
test_monad_law_right_identity
  :: ( Monad m
     , Eq a
     , Show (m a)
     , Arbitrary (m a)
     )
  => Proxy m -> Proxy a
  -> (forall u. (Eq u) => m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
test_monad_law_right_identity pm pa eq =
  testProperty "x >>= return === x" $
    monad_law_right_identity pm pa eq

monad_law_right_identity
  :: (Monad m, Eq a)
  => Proxy m -> Proxy a
  -> (forall u. (Eq u) => m u -> m u -> Bool)
  -> m a -> Bool
monad_law_right_identity _ _ eq x =
  eq (x >>= return) (x)



-- | @return a >>= f === f a@
test_monad_law_left_identity
  :: ( Monad m
     , Eq b
     , Show a
     , Arbitrary a, Arbitrary (m b)
     , CoArbitrary a
     )
  => Proxy m -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
test_monad_law_left_identity pm pa pb eq =
  testProperty "return a >>= f === f a" $
    monad_law_left_identity pm pa pb eq

monad_law_left_identity
  :: (Monad m, Eq b)
  => Proxy m -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => m u -> m u -> Bool)
  -> a -> (a -> m b) -> Bool
monad_law_left_identity _ _ _ eq x f =
  eq ((return x) >>= f) (f x)



-- | @(x >>= f) >>= g === x >>= (\\z -> f z >>= g)@
test_monad_law_associativity
  :: ( Monad m
     , Eq c
     , Show (m a)
     , Arbitrary (m a), Arbitrary (m b), Arbitrary (m c)
     , CoArbitrary a, CoArbitrary b
     )
  => Proxy m -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
test_monad_law_associativity pm pa pb pc eq =
  testProperty "(x >>= f) >>= g === x >>= (\\z -> f z >>= g)" $
    monad_law_associativity pm pa pb pc eq

monad_law_associativity
  :: ( Monad m
     , Eq c
     )
  => Proxy m -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => m u -> m u -> Bool)
  -> m a -> (a -> m b) -> (b -> m c) -> Bool
monad_law_associativity _ _ _ _ eq x f g =
  eq ((x >>= f) >>= g) (x >>= (\z -> f z >>= g))





-- | All possible selections from 1 type
test_monad_laws_1
  :: ( Monad m
     , Checkable a
     , Show (m a)
     , Arbitrary (m a)
     , Typeable m
     )
  => Proxy m -> Proxy a
  -> (forall u. (Eq u) => m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
test_monad_laws_1 pm pa eq =
  let label = "Monad Laws for " ++ (show $ typeRep pm) in
  testGroup label
    [ test_monad_laws pm pa pa pa eq
    ]



-- | All possible selections from 2 types
test_monad_laws_2
  :: ( Monad m
     , Checkable a, Checkable b
     , Show (m a), Show (m b)
     , Arbitrary (m a), Arbitrary (m b)
     , Typeable m
     )
  => Proxy m -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
test_monad_laws_2 pm pa pb eq =
  let label = "Monad Laws for " ++ (show $ typeRep pm) in
  testGroup label
    [ test_monad_laws pm pa pa pa eq
    , test_monad_laws pm pa pa pb eq
    , test_monad_laws pm pa pb pa eq
    , test_monad_laws pm pa pb pb eq
    , test_monad_laws pm pb pa pa eq
    , test_monad_laws pm pb pa pb eq
    , test_monad_laws pm pb pb pa eq
    , test_monad_laws pm pb pb pb eq
    ]



-- | All possible selections from 3 types
test_monad_laws_3
  :: ( Monad m
     , Checkable a, Checkable b, Checkable c
     , Show (m a), Show (m b), Show (m c)
     , Arbitrary (m a), Arbitrary (m b), Arbitrary (m c)
     , Typeable m
     )
  => Proxy m -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. (Eq u) => m u -> m u -> Bool) -- ^ Equality test
  -> TestTree
test_monad_laws_3 pm pa pb pc eq =
  let label = "Monad Laws for " ++ (show $ typeRep pm) in
  testGroup label
    [ test_monad_laws pm pa pa pa eq
    , test_monad_laws pm pa pa pb eq
    , test_monad_laws pm pa pa pc eq
    , test_monad_laws pm pa pb pa eq
    , test_monad_laws pm pa pb pb eq
    , test_monad_laws pm pa pb pc eq
    , test_monad_laws pm pa pc pa eq
    , test_monad_laws pm pa pc pb eq
    , test_monad_laws pm pa pc pc eq
    , test_monad_laws pm pb pa pa eq
    , test_monad_laws pm pb pa pb eq
    , test_monad_laws pm pb pa pc eq
    , test_monad_laws pm pb pb pa eq
    , test_monad_laws pm pb pb pb eq
    , test_monad_laws pm pb pb pc eq
    , test_monad_laws pm pb pc pa eq
    , test_monad_laws pm pb pc pb eq
    , test_monad_laws pm pb pc pc eq
    , test_monad_laws pm pc pa pa eq
    , test_monad_laws pm pc pa pb eq
    , test_monad_laws pm pc pa pc eq
    , test_monad_laws pm pc pb pa eq
    , test_monad_laws pm pc pb pb eq
    , test_monad_laws pm pc pb pc eq
    , test_monad_laws pm pc pc pa eq
    , test_monad_laws pm pc pc pb eq
    , test_monad_laws pm pc pc pc eq
    ]
