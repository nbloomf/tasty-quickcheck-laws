{- |
Module      : Test.Tasty.QuickCheck.Laws.Eq
Description : Prefab tasty trees of quickcheck properties for the Eq laws
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}



module Test.Tasty.QuickCheck.Laws.Eq (
    testEqLaws

  -- * Eq Laws
  , testEqLawReflexive
  , testEqLawSymmetric
  , testEqLawTransitive
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



-- | Constructs a @TestTree@ checking that the @Eq@ class laws hold for @a@.
testEqLaws
  :: (Eq a, Show a, Arbitrary a, Typeable a)
  => Proxy a
  -> TestTree
testEqLaws pa =
  let label = "Eq Laws for " ++ (show $ typeRep pa) in
  testGroup label
    [ testEqLawReflexive pa
    , testEqLawSymmetric pa
    , testEqLawTransitive pa
    ]



-- | @a == a@
testEqLawReflexive
  :: (Eq a, Show a, Arbitrary a)
  => Proxy a
  -> TestTree
testEqLawReflexive pa =
  testProperty "a == a" $
    eqLawReflexive pa

eqLawReflexive
  :: (Eq a)
  => Proxy a
  -> a -> Bool
eqLawReflexive _ a =
  a == a



-- | @(a == b) == (b == a)@
testEqLawSymmetric
  :: (Eq a, Show a, Arbitrary a)
  => Proxy a
  -> TestTree
testEqLawSymmetric pa =
  testProperty "(a == b) == (b == a)" $
    eqLawSymmetric pa

eqLawSymmetric
  :: (Eq a)
  => Proxy a
  -> a -> a -> Bool
eqLawSymmetric _ a b =
  (a == b) == (b == a)



-- | @if (a == b) && (b == c) then (a == c)@
testEqLawTransitive
  :: (Eq a, Show a, Arbitrary a)
  => Proxy a
  -> TestTree
testEqLawTransitive pa =
  testProperty "if (a == b) && (b == c) then (a == c)" $
    eqLawTransitive pa

eqLawTransitive
  :: (Eq a)
  => Proxy a
  -> a -> a -> a -> Bool
eqLawTransitive _ a b c =
  if (a == b) && (b == c) then a == c else True
