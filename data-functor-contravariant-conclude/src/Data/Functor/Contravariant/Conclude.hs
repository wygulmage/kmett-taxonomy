{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
-- for Generics:
{-# LANGUAGE DefaultSignatures
           , GADTs
           , EmptyCase
           , ScopedTypeVariables
           , Trustworthy -- lol
  #-}
#endif


module Data.Functor.Contravariant.Conclude where


import Data.Functor.Apply
import Data.Functor.Contravariant.Decide

import Data.Functor.Compose (Compose)
import Data.Functor.Contravariant
import Data.Void

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
import qualified GHC.Generics as G
#endif
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 781
import Data.Coerce (coerce)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif


class (Decide m)=> Conclude m where
    conclude :: (a -> Void) -> m a
    default conclude ::
        (gr ~ G.Rep1 m, G.Generic1 m, Conclude gr)=>
        (a -> Void) -> m a
    conclude = gconclude

concluded :: (Conclude m)=> m Void
concluded = conclude id


instance Conclude (Op r) where
    conclude f = Op (absurd . f)

instance (Apply m, Applicative m, Conclude n)=> Conclude (Compose m n)


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Conclude G.U1 where conclude _ = G.U1

instance (Conclude m)=> Conclude (G.Rec1 m) where
    conclude f = G.Rec1 (conclude f)
    {-# INLINE conclude #-}

instance (Conclude m)=> Conclude (G.M1 i info m) where
    conclude f = G.M1 (conclude f)
    {-# INLINE conclude #-}

instance (Monoid c)=> Conclude (G.K1 i c) where
    conclude _ = G.K1 mempty
    {-# INLINE conclude #-}

instance (Conclude m, Conclude n)=> Conclude ((G.:*:) m n) where
    conclude f = conclude f G.:*: conclude f
    {-# INLINE conclude #-}

instance (Apply m, Applicative m, Conclude n)=> Conclude ((G.:.:) m n) where
    conclude f = G.Comp1 (pure (conclude f))
    {-# INLINE conclude #-}

gconclude ::
    (gr ~ G.Rep1 m, G.Generic1 m, Conclude gr)=>
    (a -> Void) -> m a
gconclude f = G.to1 (conclude f)
{-# INLINE gconclude #-}

#endif
