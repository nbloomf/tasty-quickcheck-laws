{- |
Module      : Test.Tasty.QuickCheck.Laws.ErrorMonad
Description : Prefab tasty trees of quickcheck properties for error monad laws
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}



{-# LANGUAGE Rank2Types #-}
module Test.Tasty.QuickCheck.Laws.ErrorMonad (
    testErrorMonadLaws

  -- * Error Monad Laws
  , testErrorMonadLawCatchReturn
  , testErrorMonadLawCatchThrow
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
     , Eq a
     , Show t, Show e, Show a
     , Arbitrary t, Arbitrary e, Arbitrary a
     , Arbitrary (m a)
     , CoArbitrary e
     , Typeable m, Typeable e, Typeable a
     )
  => Proxy m -> Proxy t -> Proxy e -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (e -> m a) -- ^ throw
  -> (m a -> (e -> m a) -> m a) -- ^ catch
  -> TestTree
testErrorMonadLaws pm pt pe pa eq throw catch =
  let
    label = "Error Monad Laws for " ++ (show $ typeRep pm) ++ " with " ++
      "e :: " ++ (show $ typeRep pe) ++ ", " ++
      "a :: " ++ (show $ typeRep pa)
  in
    testGroup label
      [ testErrorMonadLawCatchReturn pm pt pe pa eq catch
      , testErrorMonadLawCatchThrow pm pt pe pa eq throw catch
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
  -> (e -> m a) -- ^ throw
  -> (m a -> (e -> m a) -> m a) -- ^ catch
  -> TestTree
testErrorMonadLawCatchThrow pm pt pe pa eq throw catch =
  testProperty "catch (throw e) h === h e" $
    errorMonadLawCatchThrow pm pt pe pa eq throw catch

errorMonadLawCatchThrow
  :: (Monad m, Eq a)
  => Proxy m -> Proxy t -> Proxy e -> Proxy a
  -> (forall u. (Eq u) => t -> m u -> m u -> Bool) -- ^ Equality test
  -> (e -> m a) -- ^ throw
  -> (m a -> (e -> m a) -> m a) -- ^ catch
  -> t -> e -> (e -> m a) -> Bool
errorMonadLawCatchThrow _ _ _ _ eq throw catch t e h =
  (eq t) (catch (throw e) h) (h e)
