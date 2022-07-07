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


module Data.Functor.Contravariant.Decide (
Decide (..), decided,
) where

import Data.Functor.Apply -- for Compose instance

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


class (Contravariant m)=> Decide m where
    decide :: (a -> Either b c) -> m b -> m c -> m a

    default decide ::
        (gr ~ G.Rep1 m, G.Generic1 m, Decide gr)=>
        (a -> Either b c) -> m b -> m c -> m a
    decide = gdecide

decided :: (Decide m)=> m b -> m c -> m (Either b c)
decided = decide id


instance Decide (Op r) where
    decide f (Op g) (Op h) = Op (either g h . f)


instance (Apply m, Decide n)=> Decide (Compose m n)


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Decide G.V1 where decide _ v1 v2 = v1 `seq` case v2 of {}
instance Decide G.U1 where decide _ _ _ = G.U1

instance (Decide m)=> Decide (G.Rec1 m) where
    decide = decideRec1
      where
        decideRec1 ::
            forall a b c.
            (a -> Either b c) -> G.Rec1 m b -> G.Rec1 m c -> G.Rec1 m a
        decideRec1 = coerce (decide :: (a -> Either b c) -> m b -> m c -> m a)
        {-# INLINE decideRec1 #-}
    {-# INLINE decide #-}

instance (Decide m)=> Decide (G.M1 i info m) where
    decide = decideM1
      where
        decideM1 ::
            forall a b c.
            (a -> Either b c) -> G.M1 i info m b -> G.M1 i info m c -> G.M1 i info m a
        decideM1 = coerce (decide :: (a -> Either b c) -> m b -> m c -> m a)
        {-# INLINE decideM1 #-}
    {-# INLINE decide #-}

instance (Semigroup c)=> Decide (G.K1 i c) where
    decide _ = coerce ((<>) :: c -> c -> c)
    {-# INLINE decide #-}

instance (Decide m, Decide n)=> Decide ((G.:*:) m n) where
    decide f (mx G.:*: nx) (my G.:*: ny) =
        decide f mx my G.:*: decide f nx ny
    {-# INLINABLE decide #-}

-- This creates a dependency on Apply:
instance (Apply m, Decide n)=> Decide ((G.:.:) m n) where
    decide = decideComp1
      where
        decideComp1 :: forall a b c. (a -> Either b c) -> (G.:.:) m n b -> (G.:.:) m n c -> (G.:.:) m n a
        decideComp1 f = coerce (liftF2 (decide f) :: m (n b) -> m (n c) -> m (n a))
        {-# INLINE decideComp1 #-}
    {-# INLINE decide #-}

gdecide ::
    (gr ~ G.Rep1 m, G.Generic1 m, Decide gr)=>
    (a -> Either b c) -> m b -> m c -> m a
gdecide f mx my = G.to1 (decide f (G.from1 mx) (G.from1 my))
{-# INLINE gdecide #-}

#endif

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 781
#else
coerce = unsafeCoerce
#endif
