{- |
Module      : Test.Tasty.QuickCheck.Laws.Class
Description : Convenience typeclass
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

Convenience typeclass for type signatures.
-}

module Test.Tasty.QuickCheck.Laws.Class (
    Checkable
) where

import Data.Typeable
  ( Typeable )
import Test.QuickCheck
  ( Arbitrary, CoArbitrary )

-- | Alias for convenience.
class (Eq a, Show a, Arbitrary a, CoArbitrary a, Typeable a) => Checkable a

instance Checkable ()
instance Checkable Bool
instance Checkable Int
instance Checkable Integer
instance Checkable Double
instance Checkable Char

instance (Checkable a) => Checkable [a]
instance (Checkable a) => Checkable (Maybe a)

instance (Checkable a, Checkable b) => Checkable (a,b)
instance (Checkable a, Checkable b) => Checkable (Either a b)

instance (Checkable a, Checkable b, Checkable c) => Checkable (a,b,c)
