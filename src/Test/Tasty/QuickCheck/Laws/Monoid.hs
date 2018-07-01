{- |
Module      : Test.Tasty.QuickCheck.Laws.Monoid
Description : Prefab tasty trees of quickcheck properties for the Monoid laws
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}



module Test.Tasty.QuickCheck.Laws.Monoid (
    testMonoidLaws

  -- * Monoid Laws
  , testMonoidLawIdentity
  , testMonoidLawAssociative
) where

import Data.Proxy
  ( Proxy(..) )
import Data.Typeable
  ( Typeable, typeRep )
import Test.Tasty
  ( TestTree, testGroup )
import Test.Tasty.QuickCheck
  ( testProperty, Property, Arbitrary(..) )

import Test.Tasty.QuickCheck.Laws.Class



testMonoidLaws
  :: (Monoid a, Eq a, Show a, Arbitrary a, Typeable a)
  => Proxy a
  -> TestTree
testMonoidLaws pa =
  let label = "Monoid Laws for " ++ (show $ typeRep pa) in
  testGroup label
    [ testMonoidLawIdentity pa
    , testMonoidLawAssociative pa
    ]





-- | @mappend mempty a === mappend a mempty === a@
testMonoidLawIdentity
  :: (Monoid a, Eq a, Show a, Arbitrary a)
  => Proxy a
  -> TestTree
testMonoidLawIdentity pa =
  testProperty "mempty <> a === mappend a mempty === a" $
    monoidLawIdentity pa

monoidLawIdentity
  :: (Monoid a, Eq a)
  => Proxy a
  -> a -> Bool
monoidLawIdentity _ a =
  (mappend mempty a == a) && (mappend a mempty == a)



-- | @mappend (mappend a b) c === mappend a (mappend b c)@
testMonoidLawAssociative
  :: (Monoid a, Eq a, Show a, Arbitrary a)
  => Proxy a
  -> TestTree
testMonoidLawAssociative pa =
  testProperty "mappend (mappend a b) c === mappend a (mappend b c)" $
    monoidLawAssociative pa

monoidLawAssociative
  :: (Monoid a, Eq a)
  => Proxy a
  -> a -> a -> a -> Bool
monoidLawAssociative _ a b c =
  mappend (mappend a b) c == mappend a (mappend b c)
