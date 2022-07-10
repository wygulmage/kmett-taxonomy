-- WARNING: This module contains an orphan instance of Monad for GHC.Generics.:.:
{-# LANGUAGE CPP
           , ScopedTypeVariables
           , DefaultSignatures
           , GADTs -- for default instance signature
  #-}


module Data.Distributive where

import Control.Monad (join)
import Data.Functor.Identity (Identity (..))
import Data.Coerce
import Data.Proxy
#if __GLASGOW_HASKELL__ >= 702
import qualified GHC.Generics as G
#endif

import Data.Tagged



class (Monad m)=> Distributive m where
{-^ In Haskell, every @Distributive@ functor is a @Representable@ functor, which means it admits a unique instance of @Monad@ (And, for that matter, 'MonadZip').
-}
    collect :: (Functor n)=> (a -> m b) -> n a -> m (n b)

    default collect ::
        (g ~ G.Rep1 m, G.Generic1 m, Distributive g, Functor n)=>
        (a -> m b) -> n a -> m (n b)
    collect = gcollect
    {-# INLINABLE collect #-}

distribute :: (Distributive m, Functor n)=> n (m a) -> m (n a)
distribute = collect id


instance Distributive ((->) c) where
    collect f nx y = fmap (`f` y) nx
    {-# INLINE collect #-}

instance Distributive Identity where
    collect = coerce (fmap :: (a -> b) -> m a -> m b)
        :: forall m a b. (Functor m)=> (a -> Identity b) -> m a -> Identity (m b)
    {-# INLINE collect #-}

instance Distributive Proxy where
    collect _ _ = Proxy

instance Distributive (Tagged t) where
    collect = coerce (fmap :: (a -> b) -> m a -> m b)
        :: forall m a b. (Functor m)=> (a -> Tagged t b) -> m a -> Tagged t (m b)
    {-# INLINE collect #-}


#if __GLASGOW_HASKELL__ >= 702
--- Generics ---
instance Distributive G.U1 where
    collect _ _ = G.U1

instance Distributive G.Par1 where
    collect = coerce (fmap :: (a -> b) -> m a -> m b)
      :: forall m a b. (Functor m)=> (a -> G.Par1 b) -> m a -> G.Par1 (m b)
    {-# INLINE collect #-}

instance (Distributive m)=> Distributive (G.Rec1 m) where
    collect = coerce (collect :: (a -> m b) -> n a -> m (n b))
        :: forall n a b. (Functor n)=> (a -> G.Rec1 m b) -> n a -> G.Rec1 m (n b)
    {-# INLINE collect #-}

instance (Distributive m)=> Distributive (G.M1 i meta m) where
    collect = coerce (collect :: (a -> m b) -> n a -> m (n b))
        :: forall n a b. (Functor n)=> (a -> G.M1 i meta m b) -> n a -> G.M1 i meta m (n b)
    {-# INLINE collect #-}

instance (Distributive m, Distributive n)=> Distributive ((G.:*:) m n) where
    collect f = distributePair . fmap f
      where
        distributePair omnx =
            collect (\ (mx G.:*: _) -> mx) omnx G.:*:
            collect (\ (_ G.:*: nx) -> nx) omnx
        {-# INLINE distributePair #-}
    {-# INLINE collect #-}

-- WARNING: ORPHAN INSTANCE
instance {-# OVERLAPPABLE #-} (Distributive m, Monad n)=> Monad ((G.:.:) m n) where
    G.Comp1 mnx >>= f =
        G.Comp1 $ fmap join $ mnx >>= collect (coerce f)
    {-# INLINE (>>=) #-}

instance (Distributive m, Distributive n)=> Distributive ((G.:.:) m n) where
    collect f = G.Comp1 . fmap distribute . collect (coerce f)
    {-# INLINABLE collect #-}

gcollect ::
    (g ~ G.Rep1 m, G.Generic1 m, Distributive g, Functor n)=>
    (a -> m b) -> n a -> m (n b)
gcollect f = G.to1 . collect (G.from1 . f)
{-# INLINE gcollect #-}

#endif
