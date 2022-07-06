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
Comonad (..), extend, duplicate,
) where


import Data.Functor.Extend

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


class (Extend m)=> Comonad m where
    extract :: m a -> a

#if __GLASGOW_HASKELL__ >= 720
    default extract ::
        (g ~ G.Rep1 m, G.Generic1 m, Comonad g)=> m a -> a
    extract = gextract
#endif

extend :: (Comonad m)=> (m a -> b) -> m a -> m b
extend = extended
{-# INLINE extend #-}

duplicate :: (Comonad m)=> m a -> m (m a)
duplicate = duplicated
{-# INLINE duplicate #-}

instance Comonad Identity where extract = runIdentity

instance Comonad NonEmpty where extract (x :| _) = x

instance (Monoid c)=> Comonad ((->) c) where
    extract f = f mempty
    {-# INLINABLE extract #-}

instance Comonad ((,) c) where
    extract = snd


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Comonad G.Par1 where extract = coerce

instance (Comonad m)=> Comonad (G.Rec1 m) where
    extract = extractRec1
      where
        extractRec1 :: forall a. G.Rec1 m a -> a
        extractRec1 = coerce (extract :: m a -> a)
        {-# INLINE extractRec1 #-}
    {-# INLINE extract #-}

instance (Comonad m)=> Comonad (G.M1 i info m) where
    extract = extractM1
      where
        extractM1 :: forall a. G.M1 i info m a -> a
        extractM1 = coerce (extract :: m a -> a)
        {-# INLINE extractM1 #-}
    {-# INLINE extract #-}

gextract ::
    (g ~ G.Rep1 m, G.Generic1 m, Comonad g)=> m a -> a
gextract = extract . G.from1
{-# INLINE gextract #-}

#endif
