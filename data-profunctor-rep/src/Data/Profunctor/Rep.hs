{-# LANGUAGE FlexibleContexts
           , RankNTypes
  #-}

module Data.Profunctor.Rep where


import Data.Profunctor.Sieve
import Data.Profunctor.Strong
import Data.Profunctor.Types.Star

import Data.Functor.Identity

import Data.Coerce (coerce)


class (Strong p, Sieve p)=> Representable p where
    tabulate :: (a -> Rep p b) -> p a b


instance (Functor m)=> Representable (Star m) where
    tabulate = Star


instance Representable (->) where
    tabulate = coerce


secondRep :: (Representable p)=> p a b -> p (c, a) (c, b)
secondRep = wanderRep (\ f (u, x) -> (,) u `fmap` f x)
{-# INLINE secondRep #-}

rightRep ::
    (Representable p, Applicative (Rep p))=> p a b -> p (Either c a) (Either c b)
rightRep = wanderRep traverse
{-# INLINE rightRep #-}

wanderRep ::
    (Representable p)=> ((a -> Rep p b) -> s -> Rep p t) -> p a b -> p s t
wanderRep f = tabulate . f . sieve
{-# INLINE wanderRep #-}
