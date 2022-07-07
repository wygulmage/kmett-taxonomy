{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
-- for Generics:
{-# LANGUAGE DefaultSignatures
           , GADTs
           , ScopedTypeVariables
           , Trustworthy -- lol
  #-}
#endif

module Control.Comonad (
Comonad (..), gextend,
) where


import Data.Functor.Extend
import Data.Tagged

import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty((:|)))
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
import qualified GHC.Generics as G
#endif
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 781
import Data.Coerce
#else
import Unsafe.coerce
coerce = unsafeCoerce
#endif


class (Functor m)=> Comonad m where
    {-# MINIMAL extend | duplicate #-}
    extract :: m a -> a

    extend :: (m a -> b) -> m a -> m b
    extend f = fmap f . duplicate

    duplicate :: m a -> m (m a)
    duplicate = extend id

    default extract :: (g ~ G.Rep1 m, G.Generic1 m, Comonad g)=> m a -> a
    extract = gextract


instance Comonad Identity where
    extract = runIdentity
    extend = coerce

instance Comonad NonEmpty where
    extract (x :| _) = x
    extend f xs@(x :| xs') = f xs :| loop xs'
      where
        loop (y : ys) = f (y :| ys) : loop ys
        loop [] = []

instance (Monoid c)=> Comonad ((->) c) where
    extract f = f mempty
    {-# INLINABLE extract #-}
    extend f g x = f (g . (<>) x)
    {-# INLINABLE extend #-}

instance Comonad ((,) c) where
    extract = snd
    extend f ux = (fst ux, f ux)

instance Comonad (Tagged t) where
    extract = unTagged
    extend = coerce


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Comonad G.Par1 where
    extract = coerce
    extend = coerce

instance (Comonad m)=> Comonad (G.Rec1 m) where
    extract = extractRec1
      where
        extractRec1 :: forall a. G.Rec1 m a -> a
        extractRec1 = coerce (extract :: m a -> a)
        {-# INLINE extractRec1 #-}
    {-# INLINE extract #-}
    extend = extendRec1
      where
        extendRec1 :: forall a b. (G.Rec1 m a -> b) -> G.Rec1 m a -> G.Rec1 m b
        extendRec1 = coerce (extend :: (m a -> b) -> m a -> m b)
        {-# INLINE extendRec1 #-}
    {-# INLINE extend #-}

instance (Comonad m)=> Comonad (G.M1 i info m) where
    extract = extractM1
      where
        extractM1 :: forall a. G.M1 i info m a -> a
        extractM1 = coerce (extract :: m a -> a)
        {-# INLINE extractM1 #-}
    {-# INLINE extract #-}
    extend = extendM1
      where
        extendM1 :: forall a b. (G.M1 i info m a -> b) -> G.M1 i info m a -> G.M1 i info m b
        extendM1 = coerce (extend :: (m a -> b) -> m a -> m b)
        {-# INLINE extendM1 #-}
    {-# INLINE extend #-}

instance (Comonad m, Comonad n)=> Comonad ((G.:+:) m n) where
    extract mnx = case mnx of
        G.L1 mx -> extract mx
        G.R1 nx -> extract nx
    {-# INLINABLE extract #-}
    extend f mnx = case mnx of
        G.L1 mx -> G.L1 (extend (f . G.L1) mx)
        G.R1 nx -> G.R1 (extend (f . G.R1) nx)
    {-# INLINABLE extend #-}

gextract ::
    (g ~ G.Rep1 m, G.Generic1 m, Comonad g)=> m a -> a
gextract = extract . G.from1
{-# INLINE gextract #-}

gextend :: (g ~ G.Rep1 m, G.Generic1 m, Extend g)=> (m a -> b) -> m a -> m b
gextend f = G.to1 . extended (f . G.to1) . G.from1
{-# INLINE gextend #-}

#endif
