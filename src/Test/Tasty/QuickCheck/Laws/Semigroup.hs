{- |
Module      : Test.Tasty.QuickCheck.Laws.Semigroup
Description : Prefab tasty trees of quickcheck properties for the Semigroup laws
Copyright   : 2019, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}



module Test.Tasty.QuickCheck.Laws.Semigroup (
    testSemigroupLaws

  -- * Semigroup Laws
  , testSemigroupLawAssociative
) where

import Data.Proxy
  ( Proxy(..) )
import Data.Semigroup
  ( Semigroup(..) )
import Data.Typeable
  ( Typeable, typeRep )
import Test.Tasty
  ( TestTree, testGroup )
import Test.Tasty.QuickCheck
  ( testProperty, Property, Arbitrary(..) )

import Test.Tasty.QuickCheck.Laws.Class



-- | Constructs a @TestTree@ checking that the @Semigroup@ class laws hold for @a@.
testSemigroupLaws
  :: (Semigroup a, Eq a, Show a, Arbitrary a, Typeable a)
  => Proxy a
  -> TestTree
testSemigroupLaws pa =
  let label = "Semigroup Laws for " ++ (show $ typeRep pa) in
  testGroup label
    [ testSemigroupLawAssociative pa
    ]





-- | @(a <> b) <> c === a <> (b <> c)@
testSemigroupLawAssociative
  :: (Semigroup a, Eq a, Show a, Arbitrary a)
  => Proxy a
  -> TestTree
testSemigroupLawAssociative pa =
  testProperty "(a <> b) <> c === a <> (b <> c)" $
    semigroupLawAssociative pa

semigroupLawAssociative
  :: (Semigroup a, Eq a)
  => Proxy a
  -> a -> a -> a -> Bool
semigroupLawAssociative _ a b c =
  (a <> b) <> c == a <> (b <> c)
