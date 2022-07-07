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


module Data.Functor.Contravariant.Divise (
Divise (..),
) where


import Data.Functor.Apply -- for Compose instance

import Data.Bifunctor (bimap)
import Data.Functor.Contravariant
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
import qualified GHC.Generics as G
#endif
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 781
import Data.Coerce (coerce)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif


class (Contravariant m)=> Divise m where
    divise :: (a -> (b, c)) -> m b -> m c -> m a

    default divise ::
            (gr ~ G.Rep1 m, G.Generic1 m, Divise gr)=>
            (a -> (b, c)) -> m b -> m c -> m a
    divise = gdivise

divised :: (Divise m)=> m a -> m b -> m (a, b)
divised = divise id


instance (Semigroup r)=> Divise (Op r) where
    divise f (Op g) (Op h) = Op ((\ (x, y) -> g x <> h y) . f)
    {-# INLINABLE divise #-}

instance (Apply m, Divise n)=> Divise (Compose m n)


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Divise G.V1 where divise _ v1 v2 = v1 `seq` case v2 of {}
instance Divise G.U1 where divise _ _ _ = G.U1

instance (Divise m)=> Divise (G.Rec1 m) where
    divise = diviseRec1
      where
        diviseRec1 ::
            forall a b c.
            (a -> (b, c)) -> G.Rec1 m b -> G.Rec1 m c -> G.Rec1 m a
        diviseRec1 = coerce (divise :: (a -> (b, c)) -> m b -> m c -> m a)
        {-# INLINE diviseRec1 #-}
    {-# INLINE divise #-}

instance (Divise m)=> Divise (G.M1 i info m) where
    divise = diviseM1
      where
        diviseM1 ::
            forall a b c.
            (a -> (b, c)) -> G.M1 i info m b -> G.M1 i info m c -> G.M1 i info m a
        diviseM1 = coerce (divise :: (a -> (b, c)) -> m b -> m c -> m a)
        {-# INLINE diviseM1 #-}
    {-# INLINE divise #-}

instance (Semigroup c)=> Divise (G.K1 i c) where
    divise _ = coerce ((<>) :: c -> c -> c)
    {-# INLINE divise #-}

instance (Divise m, Divise n)=> Divise ((G.:*:) m n) where
    divise f (mx G.:*: nx) (my G.:*: ny) =
        divise f mx my G.:*: divise f nx ny
    {-# INLINABLE divise #-}

-- This creates a dependency on Apply:
instance (Apply m, Divise n)=> Divise ((G.:.:) m n) where
    divise = diviseComp1
      where
        diviseComp1 :: forall a b c. (a -> (b, c)) -> (G.:.:) m n b -> (G.:.:) m n c -> (G.:.:) m n a
        diviseComp1 f = coerce (liftF2 (divise f) :: m (n b) -> m (n c) -> m (n a))
        {-# INLINE diviseComp1 #-}
    {-# INLINE divise #-}


gdivise ::
        (gr ~ G.Rep1 m, G.Generic1 m, Divise gr)=>
        (a -> (b, c)) -> m b -> m c -> m a
gdivise f mx my = G.to1 (divise f (G.from1 mx) (G.from1 my))
{-# INLINE gdivise #-}

#endif
