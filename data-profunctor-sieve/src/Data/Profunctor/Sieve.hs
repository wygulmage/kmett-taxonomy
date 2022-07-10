{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Data.Profunctor.Sieve where


import Data.Profunctor.Internal
import Data.Profunctor.Types.Star

import Data.Functor.Identity

import Data.Coerce (coerce)


class (Profunctor p, Functor (Rep p))=> Sieve p where
    type Rep p :: * -> *
    sieve :: p a b -> a -> Rep p b


instance (Functor m)=> Sieve (Star m) where
    type Rep (Star m) = m
    sieve = runStar

instance Sieve (->) where
    type Rep (->) = Identity
    sieve = coerce
