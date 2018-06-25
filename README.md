tasty-quickcheck-laws
=====================

[![Build Status](https://travis-ci.org/nbloomf/tasty-quickcheck-laws.svg?branch=master)](https://travis-ci.org/nbloomf/tasty-quickcheck-laws)

Pre-built tasty test trees for common properties using QuickCheck

I like to build my monad transformer stacks by hand. It's good exercise and helps me avoid baddies like `UndecidableInstances` and `MultiParamTypeClasses`, which I dislike because they tend to make type error messages less obvious. Testing laws with QuickCheck is a handy way to verify that hairier functor/applicative/monad instances are correct. I also like to use the tasty testing framework.

This module provides some prefab test trees using QuickCheck to test various laws, so rather than rewriting tests for, say, the monad laws all the time, we can just call `testMonadLaws` with a few `Proxy`s and get all that business for free.

Currently includes laws for the following type classes:

* Functor
* Applicative
* Monad
* Eq
* Monoid
