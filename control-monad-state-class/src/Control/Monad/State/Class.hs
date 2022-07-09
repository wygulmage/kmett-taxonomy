{-# LANGUAGE CPP
           , FunctionalDependencies
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , ScopedTypeVariables
           , DefaultSignatures
  #-}


module Control.Monad.State.Class where

import Data.Profunctor.Types.Star

import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
import qualified GHC.Generics as G
#endif
import Data.Coerce (coerce)


class (Monad m)=> MonadState s m | m -> s where
    state :: (s -> (a, s)) -> m a
    state f = gets f >>= \ (x, s) -> x <$ put s
    {-# INLINABLE state #-}

    gets :: (s -> a) -> m a
    gets f = state (\ s -> (f s, s))
    {-# INLINABLe gets #-}

    put :: s -> m ()
    put s = state (\_-> ((), s))
    {-# INLINABLe put #-}


instance (Monad m)=> MonadState s (Lazy.StateT s m) where
    state = Lazy.state
    {-# INLINE state #-}

instance (Monad m)=> MonadState s (Strict.StateT s m) where
    state = Strict.state
    {-# INLINE state #-}


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance (MonadState s m)=> MonadState s (G.Rec1 m) where
    state = coerce (state :: (s -> (a, s)) -> m a) :: forall a. (s -> (a, s)) -> G.Rec1 m a
    {-# INLINABLE state #-}
    gets = coerce (gets :: (s -> a) -> m a) :: forall a. (s -> a) -> G.Rec1 m a
    {-# INLINABLE gets #-}
    put = coerce (put :: s -> m ())
    {-# INLINABLE put #-}

gstate :: (G.Generic1 m, MonadState s (G.Rep1 m))=> (s -> (a, s)) -> m a
gstate = G.to1 . state
{-# INLINE gstate #-}
#endif
