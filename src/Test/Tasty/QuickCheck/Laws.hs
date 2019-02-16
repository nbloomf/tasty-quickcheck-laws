{- |
Module      : Test.Tasty.QuickCheck.Laws
Description : Prefab tasty trees of quickcheck properties for lawful type classes
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

module Test.Tasty.QuickCheck.Laws (
    module Test.Tasty.QuickCheck.Laws.Applicative
  , module Test.Tasty.QuickCheck.Laws.Class
  , module Test.Tasty.QuickCheck.Laws.Eq
  , module Test.Tasty.QuickCheck.Laws.ErrorMonad
  , module Test.Tasty.QuickCheck.Laws.Functor
  , module Test.Tasty.QuickCheck.Laws.Monad
  , module Test.Tasty.QuickCheck.Laws.Monoid
  , module Test.Tasty.QuickCheck.Laws.Semigroup
  , module Test.Tasty.QuickCheck.Laws.ReaderMonad
  , module Test.Tasty.QuickCheck.Laws.StateMonad
  , module Test.Tasty.QuickCheck.Laws.WriterMonad
  , module Test.Tasty.QuickCheck.Laws.MaybeMonad
  , module Test.Tasty.QuickCheck.Laws.IdentityMonad
) where

import Test.Tasty.QuickCheck.Laws.Applicative
import Test.Tasty.QuickCheck.Laws.Class
import Test.Tasty.QuickCheck.Laws.Eq
import Test.Tasty.QuickCheck.Laws.ErrorMonad
import Test.Tasty.QuickCheck.Laws.Functor
import Test.Tasty.QuickCheck.Laws.Monad
import Test.Tasty.QuickCheck.Laws.Monoid
import Test.Tasty.QuickCheck.Laws.Semigroup
import Test.Tasty.QuickCheck.Laws.ReaderMonad
import Test.Tasty.QuickCheck.Laws.StateMonad
import Test.Tasty.QuickCheck.Laws.WriterMonad
import Test.Tasty.QuickCheck.Laws.MaybeMonad
import Test.Tasty.QuickCheck.Laws.IdentityMonad
