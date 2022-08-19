{-# LANGUAGE RankNTypes
  #-}
-- Because this requires RankNTypes, it shouldn't be a dependency of modules that don't require RankNTypes.

module Control.Monad.Codensity (
Codensity, liftCodensity, liftCodensity_, lowerCodensity,
) where

import Control.Monad.Trans.Class

import Control.Applicative
import Control.Monad

import Data.Foldable

newtype Codensity m a = Codensity (forall r. (a -> m r) -> m r)
{-^ @Codensity m@ is a free 'Monad' of any @m@. It provides more power than @m@ -- for example, even if @m@ is only 'Applicative', it can be used as a 'Monad' for effects only (ignoring the value) by lifting into @Codensity@. -}
runCodensity :: Codensity m a -> (a -> m r) -> m r
runCodensity (Codensity k) = k

liftCodensity :: (Monad m)=> m a -> Codensity m a
{-^ Lift a 'Monadic' value. -}
liftCodensity mx = Codensity $ (>>=) mx
{-# INLINE liftCodensity #-}

liftCodensity_ :: (Applicative m)=> m a -> Codensity m ()
{-^ Lift an 'Applicative' action for effects only.
@'lowerCodensity' ('liftCodensity_' mx)@ is equivalent to @mx *> 'pure' ()@.
-}
liftCodensity_ mx = Codensity $ \ g -> mx *> g ()
{-# INLINE liftCodensity_ #-}

lowerCodensity :: (Applicative m)=> Codensity m a -> m a
lowerCodensity = (`runCodensity` pure)
{-# INLINE lowerCodensity #-}


instance MonadTrans Codensity where
   lift = liftCodensity
   {-# INLINE lift #-}

instance (MonadPlus m)=> MonadPlus (Codensity m)

instance Monad (Codensity m) where
   mx >>= f = Codensity $ \ g -> runCodensity mx $ \ x -> runCodensity (f x) g
   {-# INLINE (>>=) #-}
   (>>) = (*>)
   {-# INLINE (>>) #-}

instance (Alternative m)=> Alternative (Codensity m) where
   empty = Codensity $ \_-> empty
   {-# INLINE empty #-}
   mx <|> my = Codensity $ \ g -> runCodensity mx g <|> runCodensity my g
   {-# INLINE (<|>) #-}

instance Applicative (Codensity m) where
   pure x = Codensity ($ x)
   {-# INLINE pure #-}
   -- liftA2 = liftM2
   liftA2 f mx my = mx >>= \ x -> fmap (f x) my
   {-# INLINE liftA2 #-}
   -- (<*>) = ap
   mf <*> mx = mf >>= (`fmap` mx)
   {-# INLINE (<*>) #-}
   mx *> my = Codensity $ \ g -> runCodensity mx $ \_-> runCodensity my g
   {-# INLINE (*>) #-}

instance Functor (Codensity m) where
   -- fmap = liftM
   fmap f mx = Codensity $ \ g -> runCodensity mx $ g . f
   {-# INLINE fmap #-}

instance (Applicative m, Foldable m)=> Foldable (Codensity m) where
    fold = fold . lowerCodensity
    foldMap f = fold . (`runCodensity` (pure . f))
    foldl f z = foldl f z . lowerCodensity
    foldr f z = foldr f z . lowerCodensity
    foldl' f z = foldl' f z . lowerCodensity
    foldr' f z = foldr' f z . lowerCodensity
    foldl1 f = foldl1 f . lowerCodensity
    foldr1 f = foldr1 f . lowerCodensity
    length = length . lowerCodensity
    elem x = elem x . lowerCodensity
    maximum = maximum . lowerCodensity
    minimum = minimum . lowerCodensity
