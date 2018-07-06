{- |
Module      : Test.Tasty.QuickCheck.Laws.WriterMonad
Description : Prefab tasty trees of quickcheck properties for writer monad laws
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

This module uses a weakened set of primitives for writer monads which give us most of what we want but, in this author's opinion, satisfy tidier properties. In addition to the usual @tell :: (Monoid w) => w -> m ()@, we use the following.

@consume :: m a -> m (a,w)@, which is similar to @listen@ but resets the writer value to @mempty@. We can write @listen@ in terms of @consume@ and @tell@, and can write @consume@ in terms of @listen@ and @censor@.

@edit :: m (w -> w) -> m a -> m a@, which is a weak form of @pass@ (we can define @edit@ in terms of @pass@, but not vice versa) that does most of what we want from @pass@ in practice but is more obviously dual to @local@ for reader monads. :)
-}



{-# LANGUAGE Rank2Types #-}
module Test.Tasty.QuickCheck.Laws.WriterMonad (
    testWriterMonadLaws

  -- * Writer Monad Laws
  , testWriterMonadLawConsumeTell
  , testWriterMonadLawEditId
  , testWriterMonadLawEditTell
  , testWriterMonadLawEditEdit
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



testWriterMonadLaws
  :: ( Monad m
     , Eq w, Eq a
     , Show t, Show w
     , Show (m a), Show (m (w -> w))
     , Arbitrary t, Arbitrary w
     , Arbitrary (m a), Arbitrary (m (w -> w))
     , CoArbitrary w
     , Typeable m, Typeable w, Typeable a
     )
  => Proxy m -> Proxy t -> Proxy w -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ tell
  -> (forall u. m u -> m (u,w)) -- ^ consume
  -> (forall u. m (w -> w) -> m u -> m u) -- ^ edit
  -> TestTree
testWriterMonadLaws pm pt pw pa eq tell consume edit =
  let
    label = "Writer Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "w :: " ++ (show $ typeRep pw) ++ ", " ++
      "a :: " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testWriterMonadLawConsumeTell pm pt pw eq tell consume
      , testWriterMonadLawEditId pm pt pw pa eq edit
      , testWriterMonadLawEditTell pm pt pw eq tell edit
      , testWriterMonadLawEditEdit pm pt pw pa eq edit
      ]



-- | @consume (tell w) === return ((),w)@
testWriterMonadLawConsumeTell
  :: ( Monad m
     , Eq w
     , Show t, Show w
     , Arbitrary t, Arbitrary w
     )
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ tell
  -> (forall u. m u -> m (u,w)) -- ^ consume
  -> TestTree
testWriterMonadLawConsumeTell pm pt pw eq tell consume =
  testProperty "consume (tell w) === tell w >> return ((),w)" $
    writerMonadLawConsumeTell pm pt pw eq tell consume

writerMonadLawConsumeTell
  :: (Monad m, Eq w)
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ tell
  -> (forall u. m u -> m (u,w)) -- ^ consume
  -> t -> w -> Bool
writerMonadLawConsumeTell _ _ _ eq tell consume t w =
  (eq t) (consume (tell w)) (return ((),w))



-- | @edit (pure id) x === x@
testWriterMonadLawEditId
  :: ( Monad m
     , Eq a
     , Show t
     , Show (m a)
     , Arbitrary t
     , Arbitrary (m a)
     )
  => Proxy m -> Proxy t -> Proxy w -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. m (w -> w) -> m u -> m u) -- ^ edit
  -> TestTree
testWriterMonadLawEditId pm pt pw pa eq edit =
  testProperty "edit (pure id) x === x" $
    writerMonadLawEditId pm pt pw pa eq edit

writerMonadLawEditId
  :: (Monad m, Eq a)
  => Proxy m -> Proxy t -> Proxy w -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. m (w -> w) -> m u -> m u) -- ^ edit
  -> t -> m a -> Bool
writerMonadLawEditId _ _ _ _ eq edit t x =
  (eq t) (edit (pure id) x) (x)



-- | @edit (pure f) (tell w) = tell (f w)@
testWriterMonadLawEditTell
  :: ( Monad m
     , Show t, Show w
     , Arbitrary t, Arbitrary w
     , CoArbitrary w
     )
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ tell
  -> (forall u. m (w -> w) -> m u -> m u) -- ^ edit
  -> TestTree
testWriterMonadLawEditTell pm pt pw eq tell edit =
  testProperty "edit (pure f) (tell w) === tell (f w)" $
    writerMonadLawEditTell pm pt pw eq tell edit

writerMonadLawEditTell
  :: (Monad m)
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ tell
  -> (forall u. m (w -> w) -> m u -> m u) -- ^ edit
  -> t -> (w -> w) -> w -> Bool
writerMonadLawEditTell _ _ _ eq tell edit t f w =
  (eq t) (edit (pure f) (tell w)) (tell (f w))



-- | @edit u . edit v $ x === edit ((.) <$> u <*> v) x@
testWriterMonadLawEditEdit
  :: ( Monad m
     , Eq a
     , Show t
     , Show (m a), Show (m (w -> w))
     , Arbitrary t
     , Arbitrary (m a), Arbitrary (m (w -> w))
     )
  => Proxy m -> Proxy t -> Proxy w -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. m (w -> w) -> m u -> m u) -- ^ edit
  -> TestTree
testWriterMonadLawEditEdit pm pt pw pa eq edit =
  testProperty "edit u . edit v $ x === edit ((.) <$> u <*> v)" $
    writerMonadLawEditEdit pm pt pw pa eq edit

writerMonadLawEditEdit
  :: (Monad m, Eq a)
  => Proxy m -> Proxy t -> Proxy w -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. m (w -> w) -> m u -> m u) -- ^ edit
  -> t -> m (w -> w) -> m (w -> w) -> m a -> Bool
writerMonadLawEditEdit _ _ _ _ eq edit t u v x =
  (eq t) (edit u . edit v $ x) (edit ((.) <$> u <*> v) x)
