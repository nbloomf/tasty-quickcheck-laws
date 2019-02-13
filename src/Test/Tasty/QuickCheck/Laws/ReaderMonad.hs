{- |
Module      : Test.Tasty.QuickCheck.Laws.ReaderMonad
Description : Prefab tasty trees of quickcheck properties for the reader monad laws
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}



{-# LANGUAGE Rank2Types #-}
module Test.Tasty.QuickCheck.Laws.ReaderMonad (
    testReaderMonadLaws

  -- * Reader Monad Laws
  , testReaderMonadLawLocalAsk
  , testReaderMonadLawLocalLocal
  , testReaderMonadLawLocalThenAsk
  , testReaderMonadLawLocalReturn
  , testReaderMonadLawLocalBind
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



-- | Constructs a @TestTree@ checking that the reader monad laws hold for @m@ with reader type @r@ and value types @a@ and @b@, using a given equality test for values of type @forall u. m u@. The equality context type @t@ is for constructors @m@ from which we can only extract a value within a context, such as reader-like constructors.
testReaderMonadLaws
  :: ( Monad m
     , Eq r, Eq a, Eq b
     , Show t, Show a
     , Show (m a)
     , Arbitrary t, Arbitrary r, Arbitrary a
     , Arbitrary (m a), Arbitrary (m b)
     , CoArbitrary r, CoArbitrary a
     , Typeable m, Typeable r, Typeable a
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy r -- ^ Reader type
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> m r -- ^ @ask@
  -> (forall u. (r -> r) -> m u -> m u) -- ^ @local@
  -> TestTree
testReaderMonadLaws pm pt pr pa pb eq ask local =
  let
    label = "Reader Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "r :: " ++ (show $ typeRep pr) ++ ", " ++
      "a :: " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testReaderMonadLawLocalAsk pm pt pr eq ask local
      , testReaderMonadLawLocalLocal pm pt pr pa eq local
      , testReaderMonadLawLocalThenAsk pm pt pr pa eq ask local
      , testReaderMonadLawLocalReturn pm pt pr pa eq local
      , testReaderMonadLawLocalBind pm pt pr pa pb eq local
      ]



-- | @local u ask === fmap u ask@
testReaderMonadLawLocalAsk
  :: ( Monad m
     , Eq r
     , Show t
     , Arbitrary t, Arbitrary r
     , CoArbitrary r
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy r -- ^ Reader type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> m r -- ^ @ask@
  -> (forall u. (r -> r) -> m u -> m u) -- ^ @local@
  -> TestTree
testReaderMonadLawLocalAsk pm pt pr eq ask local =
  testProperty "local u ask === fmap u ask" $
    readerMonadLawLocalAsk pm pt pr eq ask local

readerMonadLawLocalAsk
  :: (Monad m, Eq r)
  => Proxy m -> Proxy t -> Proxy r
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> m r -- ^ ask
  -> (forall u. (r -> r) -> m u -> m u) -- ^ local
  -> t -> (r -> r) -> Bool
readerMonadLawLocalAsk _ _ _ eq ask local t u =
  (eq t) (local u ask) (fmap u ask)



-- | @local u (local v x) === local (v . u) x@
testReaderMonadLawLocalLocal
  :: ( Monad m
     , Eq a
     , Show t
     , Show (m a)
     , Arbitrary t, Arbitrary r
     , Arbitrary (m a)
     , CoArbitrary r
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy r -- ^ Reader type
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. (r -> r) -> m u -> m u) -- ^ @local@
  -> TestTree
testReaderMonadLawLocalLocal pm pt pr pa eq local =
  testProperty "local u (local v x) === local (v . u) x" $
    readerMonadLawLocalLocal pm pt pr pa eq local

readerMonadLawLocalLocal
  :: (Monad m, Eq a)
  => Proxy m -> Proxy t -> Proxy r -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. (r -> r) -> m u -> m u) -- ^ local
  -> t -> (r -> r) -> (r -> r) -> m a -> Bool
readerMonadLawLocalLocal _ _ _ _ eq local t u v x =
  (eq t) (local u (local v x)) (local (v . u) x)



-- | @local u ask === fmap u ask@
testReaderMonadLawLocalThenAsk
  :: ( Monad m
     , Eq r
     , Show t
     , Show (m a)
     , Arbitrary t, Arbitrary r
     , Arbitrary (m a)
     , CoArbitrary r
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy r -- ^ Reader type
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> m r -- ^ @ask@
  -> (forall u. (r -> r) -> m u -> m u) -- ^ @local@
  -> TestTree
testReaderMonadLawLocalThenAsk pm pt pr pa eq ask local =
  testProperty "local u ask === fmap u ask" $
    readerMonadLawLocalThenAsk pm pt pr pa eq ask local

readerMonadLawLocalThenAsk
  :: (Monad m, Eq r)
  => Proxy m -> Proxy t -> Proxy r -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> m r -- ^ ask
  -> (forall u. (r -> r) -> m u -> m u) -- ^ local
  -> t -> (r -> r) -> m a -> Bool
readerMonadLawLocalThenAsk _ _ _ _ eq ask local t u x =
  (eq t) (local u x >> ask) (ask >>= \r -> local u x >> return r)



-- | @local u (return a) === return a@
testReaderMonadLawLocalReturn
  :: ( Monad m
     , Eq a
     , Show t, Show a
     , Arbitrary t, Arbitrary r, Arbitrary a
     , CoArbitrary r
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy r -- ^ Reader type
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. (r -> r) -> m u -> m u) -- ^ @local@
  -> TestTree
testReaderMonadLawLocalReturn pm pt pr pa eq local =
  testProperty "local u (return a) === return a" $
    readerMonadLawLocalReturn pm pt pr pa eq local

readerMonadLawLocalReturn
  :: (Monad m, Eq a)
  => Proxy m -> Proxy t -> Proxy r -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. (r -> r) -> m u -> m u) -- ^ local
  -> t -> (r -> r) -> a -> Bool
readerMonadLawLocalReturn _ _ _ _ eq local t u a =
  (eq t) (local u (return a)) (return a)



-- | @local u (x >>= f) === local u x >>= (local u . f)@
testReaderMonadLawLocalBind
  :: ( Monad m
     , Eq b
     , Show t, Show a
     , Show (m a)
     , Arbitrary t, Arbitrary r, Arbitrary a
     , Arbitrary (m a), Arbitrary (m b)
     , CoArbitrary r, CoArbitrary a
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy r -- ^ Reader type
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. (r -> r) -> m u -> m u) -- ^ local
  -> TestTree
testReaderMonadLawLocalBind pm pt pr pa pb eq local =
  testProperty "local u (x >>= f) === local u x >>= (local u . f)" $
    readerMonadLawLocalBind pm pt pr pa pb eq local

readerMonadLawLocalBind
  :: (Monad m, Eq b)
  => Proxy m -> Proxy t -> Proxy r -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. (r -> r) -> m u -> m u) -- ^ local
  -> t -> (r -> r) -> m a -> (a -> m b) -> Bool
readerMonadLawLocalBind _ _ _ _ _ eq local t u x f =
  (eq t) (local u (x >>= f)) (local u x >>= (local u . f))
