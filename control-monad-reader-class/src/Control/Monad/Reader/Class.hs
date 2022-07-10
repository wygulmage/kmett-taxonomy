{-# LANGUAGE CPP
           , FunctionalDependencies
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , ScopedTypeVariables
           , DefaultSignatures
  #-}

module Control.Monad.Reader.Class where

import Data.Profunctor.Types.Star

import qualified Control.Monad.Trans.Reader as Reader


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
import qualified GHC.Generics as G
#endif
import Data.Coerce (coerce)


class (Monad m)=> MonadReader i m | m -> i where
    reader :: (i -> a) -> m a
    local :: (i -> i) -> m a -> m a

    default reader :: (G.Generic1 m, GReader i (G.Rep1 m))=> (i -> a) -> m a
    reader = greader
    default local :: (G.Generic1 m, GLocal i (G.Rep1 m))=> (i -> i) -> m a -> m a
    local = glocal

ask :: (MonadReader i m)=> m i
ask = reader id

asks :: (MonadReader i m)=> (i -> a) -> m a
asks = reader
{-# INLINE asks #-}


instance MonadReader i ((->) i) where
    reader = id
    local f = (. f)

instance (Monad m)=> MonadReader i (Reader.ReaderT i m) where
    reader = Reader.reader
    local = Reader.local

instance (Monad m)=> MonadReader i (Star m i) where
    reader f = Star (pure . f)
    local f (Star g) = Star (g . f)


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
class GReader i m where greader' :: (i -> a) -> m a
class GLocal i m where glocal' :: (i -> i) -> m a -> m a

instance (MonadReader i m)=> GReader i (G.Rec1 m) where
    greader' = coerce (reader :: (i -> a) -> m a) :: forall a. (i -> a) -> G.Rec1 m a
instance (MonadReader i m)=> GLocal i (G.Rec1 m) where
    glocal' = coerce (local :: (i -> i) -> m a -> m a) :: forall a. (i -> i) -> G.Rec1 m a -> G.Rec1 m a
instance (MonadReader i m)=> MonadReader i (G.Rec1 m) where
    reader = greader'
    local = glocal'

instance (MonadReader i m)=> GReader i (G.M1 j meta m) where
    greader' = coerce (reader :: (i -> a) -> m a) :: forall a. (i -> a) -> G.M1 j meta m a
instance (MonadReader i m)=> GLocal i (G.M1 j meta m) where
    glocal' = coerce (local :: (i -> i) -> m a -> m a)
        :: forall a. (i -> i) -> G.M1 j meta m a -> G.M1 j meta m a
instance (MonadReader i m)=> MonadReader i (G.M1 j meta m) where
    reader = greader'
    local = glocal'

instance (Applicative m)=> GReader i ((G.:.:) ((->) i) m) where
    greader' f = G.Comp1 (pure . f)
    {-# INLINE greader' #-}
instance GLocal i ((G.:.:) ((->) i) m) where
    glocal' f (G.Comp1 g) = G.Comp1 (g . f)
    {-# INLINE glocal' #-}


greader :: (G.Generic1 m, GReader i (G.Rep1 m))=> (i -> r) -> m r
greader = G.to1 . greader'
{-# INLINE greader #-}

glocal :: (G.Generic1 m, GLocal i (G.Rep1 m))=> (i -> i) -> m a -> m a
glocal f = G.to1 . glocal' f . G.from1
{-# INLINE glocal #-}

#endif
