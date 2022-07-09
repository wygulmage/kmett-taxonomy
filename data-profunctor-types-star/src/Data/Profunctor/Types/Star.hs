

module Data.Profunctor.Types.Star where


import Control.Applicative


newtype Star m a b = Star (a -> m b)
runStar :: Star m a b -> a -> m b
runStar (Star f) = f


instance (Monad m)=> Monad (Star m c) where
    Star k >>= f = Star $ \ x -> do
        y <- k x
        runStar (f y) x
    {-# INLINABLE (>>=) #-}

instance (Applicative m)=> Applicative (Star m c) where
    pure x = Star (pure (pure x))
    {-# INLINABLE pure #-}
    liftA2 f (Star k1) (Star k2) = Star (liftA2 (liftA2 f) k1 k2)
    {-# INLINABLE liftA2 #-}
    Star k1 <* Star k2 = Star (liftA2 (<*) k1 k2)
    {-# INLINABLE (*>) #-}
    Star k1 *> Star k2 = Star (liftA2 (*>) k1 k2)
    {-# INLINABLE (<*) #-}

instance (Functor m)=> Functor (Star m c) where
    fmap f (Star k) = Star (fmap (fmap f) k)
    {-# INLINABLE fmap #-}
    x <$ Star k = Star ((<$) x . k)
    {-# INLINABLE (<$) #-}