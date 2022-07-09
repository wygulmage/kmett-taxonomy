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

    default reader :: (G.Generic1 m, MonadReader i (G.Rep1 m))=> (i -> a) -> m a
    reader = greader
    default local :: (G.Generic1 m, MonadReader i (G.Rep1 m))=> (i -> i) -> m a -> m a
    local = glocal


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
instance (MonadReader i m)=> MonadReader i (G.Rec1 m) where
    reader f = G.Rec1 (reader f)
    local = coerce (local :: (i -> i) -> m a -> m a) :: forall a. (i -> i) -> G.Rec1 m a -> G.Rec1 m a

instance (MonadReader i m)=> MonadReader i (G.M1 j meta m) where
    reader f = G.M1 (reader f)
    local = coerce (local :: (i -> i) -> m a -> m a)
        :: forall a. (i -> i) -> G.M1 j meta m a -> G.M1 j meta m a

greader :: (G.Generic1 m, MonadReader i (G.Rep1 m))=> (i -> r) -> m r
greader = G.to1 . reader
{-# INLINE greader #-}

glocal :: (G.Generic1 m, MonadReader i (G.Rep1 m))=> (i -> i) -> m a -> m a
glocal f = G.to1 . local f . G.from1
{-# INLINE glocal #-}

#endif
