{- |
Module      : Test.Tasty.QuickCheck.Laws.ErrorMonad
Description : Prefab tasty trees of quickcheck properties for error monad laws
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}



{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Test.Tasty.QuickCheck.Laws.ErrorMonad (
    testErrorMonadLaws

  -- * Error Monad Laws
  , testErrorMonadLawCatchReturn
  , testErrorMonadLawCatchThrow
  , testErrorMonadLawCatchThrowThrow
  , testErrorMonadLawThrowBind
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



testErrorMonadLaws
  :: ( Monad m
     , Eq a, Eq b
     , Show t, Show e, Show a
     , Arbitrary t, Arbitrary e, Arbitrary a
     , Arbitrary (m a), Arbitrary (m b)
     , CoArbitrary e, CoArbitrary a
     , Typeable m, Typeable e, Typeable a
     )
  => Proxy m -> Proxy t -> Proxy e -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. e -> m u) -- ^ throw
  -> (m a -> (e -> m a) -> m a) -- ^ catch
  -> TestTree
testErrorMonadLaws pm pt pe pa pb eq throw catch =
  let
    label = "Error Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "e :: " ++ (show $ typeRep pe) ++ ", " ++
      "a :: " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testErrorMonadLawCatchReturn pm pt pe pa eq catch
      , testErrorMonadLawCatchThrow pm pt pe pa eq throw catch
      , testErrorMonadLawCatchThrowThrow pm pt pe pa eq throw catch
      , testErrorMonadLawThrowBind pm pt pe pa pb eq throw
      ]



-- | @catch (return a) h === return a@
testErrorMonadLawCatchReturn
  :: ( Monad m
     , Eq a
     , Show t, Show a
     , Arbitrary t, Arbitrary a
     , Arbitrary (m a)
     , CoArbitrary e
     )
  => Proxy m -> Proxy t -> Proxy e -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (m a -> (e -> m a) -> m a) -- ^ catch
  -> TestTree
testErrorMonadLawCatchReturn pm pt pe pa eq catch =
  testProperty "catch (return a) h === return a" $
    errorMonadLawCatchReturn pm pt pe pa eq catch

errorMonadLawCatchReturn
  :: (Monad m, Eq a)
  => Proxy m -> Proxy t -> Proxy e -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (m a -> (e -> m a) -> m a) -- ^ catch
  -> t -> a -> (e -> m a) -> Bool
errorMonadLawCatchReturn _ _ _ _ eq catch t a h =
  (eq t) (catch (return a) h) (return a)



-- | @catch (throw e) h === h e@
testErrorMonadLawCatchThrow
  :: ( Monad m
     , Eq a
     , Show t, Show e
     , Arbitrary t, Arbitrary e
     , Arbitrary (m a)
     , CoArbitrary e
     )
  => Proxy m -> Proxy t -> Proxy e -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. e -> m u) -- ^ throw
  -> (m a -> (e -> m a) -> m a) -- ^ catch
  -> TestTree
testErrorMonadLawCatchThrow pm pt pe pa eq throw catch =
  testProperty "catch (throw e) h === h e" $
    errorMonadLawCatchThrow pm pt pe pa eq throw catch

errorMonadLawCatchThrow
  :: (Monad m, Eq a)
  => Proxy m -> Proxy t -> Proxy e -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. e -> m u) -- ^ throw
  -> (m a -> (e -> m a) -> m a) -- ^ catch
  -> t -> e -> (e -> m a) -> Bool
errorMonadLawCatchThrow _ _ _ _ eq throw catch t e h =
  (eq t) (catch (throw e) h) (h e)



-- | @catch (throw e) throw === throw e@
testErrorMonadLawCatchThrowThrow
  :: ( Monad m
     , Eq a
     , Show t, Show e
     , Arbitrary t, Arbitrary e
     )
  => Proxy m -> Proxy t -> Proxy e -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. e -> m u) -- ^ throw
  -> (m a -> (e -> m a) -> m a) -- ^ catch
  -> TestTree
testErrorMonadLawCatchThrowThrow pm pt pe pa eq throw catch =
  testProperty "catch (throw e) throw === throw e" $
    errorMonadLawCatchThrowThrow pm pt pe pa eq throw catch

errorMonadLawCatchThrowThrow
  :: (Monad m, Eq a)
  => Proxy m -> Proxy t -> Proxy e -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. e -> m u) -- ^ throw
  -> (m a -> (e -> m a) -> m a) -- ^ catch
  -> t -> e -> Bool
errorMonadLawCatchThrowThrow _ _ _ _ eq throw catch t e =
  (eq t) (catch (throw e) throw) (throw e)



-- | @throw e >>= f === throw e@
testErrorMonadLawThrowBind
  :: ( Monad m
     , Eq b
     , Show t, Show e
     , Arbitrary t, Arbitrary e
     , Arbitrary (m b)
     , CoArbitrary a
     )
  => Proxy m -> Proxy t -> Proxy e -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. e -> m u) -- ^ throw
  -> TestTree
testErrorMonadLawThrowBind pm pt pe pa pb eq throw =
  testProperty "throw e >>= f === throw e" $
    errorMonadLawThrowBind pm pt pe pa pb eq throw

errorMonadLawThrowBind
  :: (Monad m, Eq b)
  => Proxy m -> Proxy t -> Proxy e -> Proxy a -> Proxy b
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (forall u. e -> m u) -- ^ throw
  -> t -> e -> (a -> m b) -> Bool
errorMonadLawThrowBind _ _ _ _ _ eq throw t e f =
  (eq t) (throw e >>= f) (throw e)
