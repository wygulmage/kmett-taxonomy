

module Data.Profunctor.Internal where


import Data.Profunctor.Types.Star
import Data.Tagged

import Control.Arrow (Kleisli (..), runKleisli, (^<<), (^>>))
import Data.Coerce


class Profunctor p where
    {-# MINIMAL dimap | (lmap, rmap) #-}
    dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'
    dimap g f h = lmap g (rmap f h)

    lmap :: (a' -> a) -> p a b -> p a' b
    lmap f = dimap f id

    rmap :: (b -> b') -> p a b -> p a b'
    rmap = dimap id

    (#.) :: (Coercible b b')=> q b b' -> p a b -> p a b'
    (#.) _ = rmap coerce

    (.#) :: (Coercible a a')=> p a b -> q a' a -> p a' b
    (.#) f _ = lmap coerce f


instance (Functor m)=> Profunctor (Star m) where
    lmap f (Star k) = Star (k . f)
    rmap = fmap
    {-# INLINE rmap #-}
    (#.) _ (Star k) = Star (fmap coerce . k)
    {-# INLINABLE (#.) #-}
    (.#) k _ = coerce k

instance Profunctor (->) where
    lmap = flip (.)
    rmap = (.)
    (#.) _ = coerce
    (.#) f _ = coerce f

instance (Monad m)=> Profunctor (Kleisli m) where
    lmap = (^>>)
    {-# INLINE lmap #-}
    rmap = (^<<)
    {-# INLINE rmap #-}
    (.#) k _ = coerce k

instance Profunctor Tagged where
    lmap _ = coerce
    rmap = coerce
