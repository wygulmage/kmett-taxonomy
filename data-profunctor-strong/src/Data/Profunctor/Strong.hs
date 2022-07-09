

module Data.Profunctor.Strong where


import Data.Profunctor.Internal
import Data.Profunctor.Types.Star


class (Profunctor p)=> Strong p where
    second :: p a b -> p (c, a) (c, b)

instance (Applicative m)=> Strong (Star m) where
    second (Star k) = Star (\ (u, x) -> fmap ((,) u) (k x))
    {-# INLINABLE second #-}

instance Strong (->) where
    second = fmap
