{- |
Module      : Test.Tasty.QuickCheck.Laws.WriterMonad
Description : Prefab tasty trees of quickcheck properties for writer monad laws
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

This module uses an alternative set of primitives for writer monads which are roughly equivalent in power to the standard @tell@, @listen@, and @pass@, but satisfy tidier properties and have a nice intuitive interpretation. We keep @tell :: w -> m ()@ and replace @listen@ and @pass@ with @draft :: (Monoid w) => m a -> m (a,w)@, which is similar to @listen@ but 'resets' the writer value to @mempty@. Intuitively, @draft@ returns what it /would/ have written, but doesn't actually write it. It is also satisfying that @draft@ uses the monoid constraint on @w@, while @tell@ and @pass@ do not.

Since we are using nonstandard primitives, we also provide an extra tree of tests for checking the equivalence of the two sets.
-}



{-# LANGUAGE Rank2Types #-}
module Test.Tasty.QuickCheck.Laws.WriterMonad (
    testWriterMonadLaws

  -- * Writer Monad Laws
  , testWriterMonadLawDraftTell
  , testWriterMonadLawTellMempty
  , testWriterMonadLawTellMappend
  , testWriterMonadLawDraftReturn
  , testWriterMonadLawDraftBind

  -- * Alternate Primitives
  , testWriterMonadEquivalences
  , testWriterMonadEquivalenceListen
  , testWriterMonadEquivalencePass
  , testWriterMonadEquivalenceDraft
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


-- | Constructs a @TestTree@ checking that the writer monad laws hold for @m@ with writer type @w@ and value types @a@ and @b@, using a given equality test for values of type @forall u. m u@. The equality context type @t@ is for constructors @m@ from which we can only extract a value within a context, such as reader-like constructors.
-- 
-- We use a slightly different set of primitives for the writer laws; rather than @tell@, @listen@, and @pass@, we use @tell@ and @draft :: (Monoid w) => m a -> m (a,w)@, which is similar to @listen@ but 'resets' the writer value to @mempty@.
testWriterMonadLaws
  :: ( Monoid w, Monad m
     , Eq w, Eq a, Eq b
     , Show t, Show w, Show a
     , Show (m a)
     , Arbitrary t, Arbitrary w, Arbitrary a
     , Arbitrary (m a), Arbitrary (m b)
     , CoArbitrary a
     , Typeable m, Typeable w, Typeable a
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @tell@
  -> (forall u. m u -> m (u,w)) -- ^ @draft@
  -> TestTree
testWriterMonadLaws pm pt pw pa pb eq tell draft =
  let
    label = "Writer Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "w :: " ++ (show $ typeRep pw) ++ ", " ++
      "a :: " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testWriterMonadLawDraftTell pm pt pw eq tell draft
      , testWriterMonadLawTellMempty pm pt pw eq tell
      , testWriterMonadLawTellMappend pm pt pw eq tell
      , testWriterMonadLawDraftReturn pm pt pw pa eq draft
      , testWriterMonadLawDraftBind pm pt pw pa pb eq draft
      ]



-- | @draft (tell w) === return ((),w)@
testWriterMonadLawDraftTell
  :: ( Monad m
     , Eq w
     , Show t, Show w
     , Arbitrary t, Arbitrary w
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @tell@
  -> (forall u. m u -> m (u,w)) -- ^ @draft@
  -> TestTree
testWriterMonadLawDraftTell pm pt pw eq tell draft =
  testProperty "draft (tell w) === tell w >> return ((),w)" $
    writerMonadLawDraftTell pm pt pw eq tell draft

writerMonadLawDraftTell
  :: (Monad m, Eq w)
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ tell
  -> (forall u. m u -> m (u,w)) -- ^ draft
  -> t -> w -> Bool
writerMonadLawDraftTell _ _ _ eq tell draft t w =
  (eq t) (draft (tell w)) (return ((),w))



-- | @tell mempty === return ()@
testWriterMonadLawTellMempty
  :: ( Monoid w, Monad m
     , Show t
     , Arbitrary t
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @tell@
  -> TestTree
testWriterMonadLawTellMempty pm pt pw eq tell =
  testProperty "tell mempty === return ()" $
    writerMonadLawTellMempty pm pt pw eq tell

writerMonadLawTellMempty
  :: (Monoid w, Monad m)
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ tell
  -> t -> Bool
writerMonadLawTellMempty _ _ _ eq tell t =
  (eq t) (tell mempty) (return ())



-- | @tell w1 >> tell w2 === tell (mappend w1 w2)@
testWriterMonadLawTellMappend
  :: ( Monoid w, Monad m
     , Show t, Show w
     , Arbitrary t, Arbitrary w
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @tell@
  -> TestTree
testWriterMonadLawTellMappend pm pt pw eq tell =
  testProperty "tell mempty === return ()" $
    writerMonadLawTellMappend pm pt pw eq tell

writerMonadLawTellMappend
  :: (Monoid w, Monad m)
  => Proxy m -> Proxy t -> Proxy w
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ tell
  -> t -> w -> w -> Bool
writerMonadLawTellMappend _ _ _ eq tell t w1 w2 =
  (eq t) (tell w1 >> tell w2) (tell (mappend w1 w2))



-- | @draft (return a) === return (a, mempty)@
testWriterMonadLawDraftReturn
  :: ( Monoid w, Monad m
     , Eq w, Eq a
     , Show t, Show a
     , Arbitrary t, Arbitrary a
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. m u -> m (u,w)) -- ^ @draft@
  -> TestTree
testWriterMonadLawDraftReturn pm pt pw pa eq draft =
  testProperty "draft (return a) === return a" $
    writerMonadLawDraftReturn pm pt pw pa eq draft

writerMonadLawDraftReturn
  :: (Monoid w, Monad m, Eq a, Eq w)
  => Proxy m -> Proxy t -> Proxy w -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. m u -> m (u,w)) -- ^ draft
  -> t -> a -> Bool
writerMonadLawDraftReturn _ _ _ _ eq draft t a =
  (eq t) (draft (return a)) (return (a, mempty))



-- | @draft (x >>= f) === draft x >>= (draft' f)@ where @draft' f (a,w) = mapsnd (mappend w) <$> draft (f a)@
testWriterMonadLawDraftBind
  :: ( Monoid w, Monad m
     , Eq w, Eq b
     , Show t
     , Show (m a)
     , Arbitrary t
     , Arbitrary (m a), Arbitrary (m b)
     , CoArbitrary a
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> Proxy a -- ^ Value type
  -> Proxy b -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. m u -> m (u,w)) -- ^ @draft@
  -> TestTree
testWriterMonadLawDraftBind pm pt pw pa pb eq draft =
  testProperty "draft (x >>= f) === draft x >>= draft' f" $
    writerMonadLawDraftBind pm pt pw pa pb eq draft

writerMonadLawDraftBind
  :: (Monoid w, Monad m, Eq w, Eq b)
  => Proxy m -> Proxy t -> Proxy w -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. m u -> m (u,w)) -- ^ draft
  -> t -> m a -> (a -> m b) -> Bool
writerMonadLawDraftBind _ _ _ _ _ eq draft t x f =
  let mapsnd g (u,v) = (u, g v) in
  let draft' g (b,v) = mapsnd (mappend v) <$> draft (g b) in
  (eq t) (draft (x >>= f)) (draft x >>= draft' f)



-- | As far as I can tell there isn't an agreed upon set of axioms for the standard writer monad primitives, so the best we can do is show that @tell@+@draft@ is equivalent in power to @tell@+@listen@+@pass@ for the only concrete writer monad we do have: the tuple monad.
--
-- Along with the property tests (for arbitrary writer monads) we'll demonstrate these equivalences (for the tuple monad) with some equational proofs, and for this purpose we need some concrete definitions.
--
-- > data Writer w a = Writer { runWriter :: (a,w) }
--
-- Note that @Writer@ and @runWriter@ are mutual inverses.
--
-- > instance (Monoid w) => Monad (Writer w) where
-- >   return a = Writer (a, mempty)
-- >
-- >   (Writer (a,w)) >>= f =
-- >     let (b,w2) = runWriter (f a)
-- >     in Writer (b, mappend w w2)
-- >
-- > tell :: w -> Writer w ()
-- > tell w = Writer ((),w)
-- >
-- > draft :: (Monoid w) => Writer w a -> Writer w (a,w)
-- > draft (Writer (a,w)) = Writer ((a,w),mempty)
-- >
-- > listen :: Writer w a -> Writer w (a,w)
-- > listen (Writer (a,w)) = Writer ((a,w),w)
-- >
-- > pass :: Writer w (a, w -> w) -> Writer w a
-- > pass u =
-- >   let ((a,f),w) = runWriter u
-- >   in Writer (a, f w)
testWriterMonadEquivalences
  :: ( Monoid w, Monad m
     , Eq w, Eq a
     , Show t
     , Show (m a), Show (m (a, w -> w))
     , Arbitrary t
     , Arbitrary (m a), Arbitrary (m (a, w -> w))
     , Typeable m, Typeable w, Typeable a
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer monad
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @tell@
  -> (forall u. m u -> m (u,w)) -- ^ @draft@
  -> (forall u. m u -> m (u,w)) -- ^ @listen@
  -> (forall u. m (u, w -> w) -> m u) -- ^ @pass@
  -> TestTree
testWriterMonadEquivalences pm pt pw pa eq tell draft listen pass =
  let
    label = "Writer Monad Equivalence for " ++ (show $ typeRep pm) ++ " with " ++
      "w :: " ++ (show $ typeRep pw) ++ ", " ++
      "a :: " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testWriterMonadEquivalenceListen pm pt pw pa eq tell draft listen
      , testWriterMonadEquivalencePass pm pt pw pa eq tell draft pass
      , testWriterMonadEquivalenceDraft pm pt pw pa eq draft listen pass
      ]



-- | @listen x === do (a,w) <- draft x; tell w; return (a,w)@
-- 
-- Suppose @x === Writer (a,w)@. Then we have:
--
-- >     do (a,w) <- draft x; tell w; return (a,w)
-- > ===   (desugar do-notation)
-- >     draft x >>= \(a',w') -> tell w' >> return (a',w')
-- > ===   (substitute x)
-- >     draft (Writer (a,w)) >>= \(a',w') -> tell w' >> return (a',w')
-- > ===   (definition of draft)
-- >     Writer ((a,w), mempty) >>= \(a',w') -> tell w' >> return (a',w')
-- > ===   (definition of >>=)
-- >     let (b,w2) = runWriter (tell w >> return (a,w))
-- >     in Writer (b, mappend mempty w2)
-- > ===   (monoid identity law)
-- >     let (b,w2) = runWriter (tell w >> return (a,w))
-- >     in Writer (b, w2)
-- > ===   (let substitution)
-- >     Writer $ runWriter (tell w >> return (a,w))
-- > ===   (constructor/destructor)
-- >     tell w >> return (a,w)
-- > ===   (definition of tell and >>)
-- >     Writer ((),w) >>= \_ -> return (a,w)
-- > ===   (definition of >>=)
-- >     let (b,w2) = runWriter (return (a,w))
-- >     in Writer (b, mappend w w2)
-- > ===   (definition of return)
-- >     let (b,w2) = runWriter (Writer ((a,w),mempty))
-- >     in Writer (b, mappend w w2)
-- > ===   (constructor/destructor)
-- >     let (b,w2) = ((a,w),mempty)
-- >     in Writer (b, mappend w w2)
-- > ===   (let substitution)
-- >     Writer ((a,w), mappend w mempty)
-- > ===   (monoid identity law)
-- >     Writer ((a,w), w)
-- > ===   (definition of listen)
-- >     listen (Writer (a,w))
-- > ===   (substitute x)
-- >     listen x
testWriterMonadEquivalenceListen
  :: ( Monad m
     , Eq w, Eq a
     , Show t
     , Show (m a)
     , Arbitrary t
     , Arbitrary (m a)
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer monad
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @tell@
  -> (forall u. m u -> m (u,w)) -- ^ @draft@
  -> (forall u. m u -> m (u,w)) -- ^ @listen@
  -> TestTree
testWriterMonadEquivalenceListen pm pt pw pa eq tell draft listen =
  testProperty "listen x === do (a,w) <- draft x; tell w; return (a,w)" $
    writerMonadEquivalenceListen pm pt pw pa eq tell draft listen

writerMonadEquivalenceListen
  :: (Monad m, Eq a, Eq w)
  => Proxy m -> Proxy t -> Proxy w -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ tell
  -> (forall u. m u -> m (u,w)) -- ^ draft
  -> (forall u. m u -> m (u,w)) -- ^ listen
  -> t -> m a -> Bool
writerMonadEquivalenceListen _ _ _ _ eq tell draft listen t x =
  (eq t) (listen x) (do (a,w) <- draft x; tell w; return (a,w))



-- | @pass u === do ((a,f),w) <- draft u; tell (f w); return a@
--
-- Suppose @u = Writer ((a,f),w)@. Then we have:
--
-- >     do ((a,f),w) <- draft u; tell (f w); return a
-- > ===   (desugar do notation)
-- >     draft u >>= \((a',f'),w') -> tell (f' w') >> return a'
-- > ===   (substitute u)
-- >     Writer (((a,f),w),mempty) >>= \((a',f'),w') -> tell (f' w') >> return a'
-- > ===   (definition of >>=)
-- >     let (b,w2) = runWriter (tell (f w) >> return a)
-- >     in Writer (b, mappend mempty w2)
-- > ===   (monoid identity law)
-- >     let (b,w2) = runWriter (tell (f w) >> return a)
-- >     in Writer (b,w2)
-- > ===   (let substitution)
-- >     Writer $ runWriter (tell (f w) >> return a)
-- > ===   (constructor/destructor)
-- >     tell (f w) >> return a
-- > ===   (definition of tell, >>, and return)
-- >     Writer ((), f w) >>= \_ -> Writer (a, mempty)
-- > ===   (definition of >>=)
-- >     let (b,w2) = runWriter (Writer (a,mempty))
-- >     in Writer (b, mappend (f w) w2)
-- > ===   (constructor/destructor)
-- >     let (b,w2) = (a,mempty)
-- >     in Writer (b, mappend (f w) w2)
-- > ===   (let substitution)
-- >     Writer (a, mappend (f w) mempty)
-- > ===   (monoid identity law)
-- >     Writer (a, f w)
-- > ===   (definition of pass)
-- >     pass (Writer (a,f) w)
-- > ===   (substitute u)
-- >     pass u
testWriterMonadEquivalencePass
  :: ( Monad m
     , Eq w, Eq a
     , Show t
     , Show (m (a, w -> w))
     , Arbitrary t
     , Arbitrary (m (a, w -> w))
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ @tell@
  -> (forall u. m u -> m (u,w)) -- ^ @draft@
  -> (forall u. m (a, w -> w) -> m a) -- ^ @pass@
  -> TestTree
testWriterMonadEquivalencePass pm pt pw pa eq tell draft pass =
  testProperty "pass u === do ((a,f),w) <- draft u; tell (f w); return a" $
    writerMonadEquivalencePass pm pt pw pa eq tell draft pass

writerMonadEquivalencePass
  :: (Monad m, Eq a, Eq w)
  => Proxy m -> Proxy t -> Proxy w -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (w -> m ()) -- ^ tell
  -> (forall u. m u -> m (u,w)) -- ^ draft
  -> (forall u. m (a, w -> w) -> m a) -- ^ pass
  -> t -> m (a, w -> w) -> Bool
writerMonadEquivalencePass _ _ _ _ eq tell draft pass t u =
  (eq t) (pass u) (do ((a,f),w) <- draft u; tell (f w); return a)



-- | @draft x === pass $ do (a,w) <- listen x; return ((a,w), const mempty)@
--
-- Suppose @x = Writer (a,w)@. Then we have:
--
-- >     pass $ do (a,w) <- listen x; return ((a,w), const mempty)
-- > ===   (desugar do notation)
-- >     pass $ listen x >>= \(a',w') -> return ((a',w'), const mempty)
-- > ===   (substitute x)
-- >     pass $ listen (Writer (a,w)) >>= \(a',w') -> return ((a',w'), const mempty)
-- > ===   (definition of listen)
-- >     pass $ Writer ((a,w),w) >>= \(a',w') -> return ((a',w'), const mempty)
-- > ===   (definition of >>=)
-- >     pass $ let (b,w2) = runWriter (return ((a,w), const mempty))
-- >            in Writer (b, mappend w w2)
-- > ===   (definition of return)
-- >     pass $ let (b,w2) = runWriter (Writer (((a,w), const mempty), mempty))
-- >            in Writer (b, mappend w w2)
-- > ===   (constructor/destructor)
-- >     pass $ let (b,w2) = (((a,w), const mempty), mempty)
-- >            in Writer (b, mappend w w2)
-- > ===   (let substitution)
-- >     pass $ Writer (((a,w), const mempty), mappend w mempty)
-- > ===   (monoid identity law)
-- >     pass $ Writer (((a,w), const mempty), w)
-- > ===   (definition of pass)
-- >     Writer ((a,w), const mempty w)
-- > ===   (definition of const)
-- >     Writer ((a,w), mempty)
-- > ===   (definition of draft)
-- >     draft (Writer (a,w))
-- > ===   (substitute x)
-- >     draft x
testWriterMonadEquivalenceDraft
  :: ( Monoid w, Monad m
     , Eq w, Eq a
     , Show t
     , Show (m a)
     , Arbitrary t
     , Arbitrary (m a)
     )
  => Proxy m -- ^ Type constructor under test
  -> Proxy t -- ^ Equality context for @m@
  -> Proxy w -- ^ Writer type
  -> Proxy a -- ^ Value type
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. m u -> m (u,w)) -- ^ @draft@
  -> (forall u. m u -> m (u,w)) -- ^ @listen@
  -> (forall u. m (u, w -> w) -> m u) -- ^ @pass@
  -> TestTree
testWriterMonadEquivalenceDraft pm pt pw pa eq draft listen pass =
  testProperty "draft x === pass $ do (a,w) <- listen x; return ((a,w), const mempty)" $
    writerMonadEquivalenceDraft pm pt pw pa eq draft listen pass

writerMonadEquivalenceDraft
  :: (Monoid w, Monad m, Eq a, Eq w)
  => Proxy m -> Proxy t -> Proxy w -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. m u -> m (u,w)) -- ^ draft
  -> (forall u. m u -> m (u,w)) -- ^ listen
  -> (forall u. m (u, w -> w) -> m u) -- ^ pass
  -> t -> m a -> Bool
writerMonadEquivalenceDraft _ _ _ _ eq draft listen pass t x =
  (eq t) (draft x) (pass $ do (a,w) <- listen x; return ((a,w), const mempty))
