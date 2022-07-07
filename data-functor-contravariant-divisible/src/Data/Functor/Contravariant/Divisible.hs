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

module Data.Functor.Contravariant.Divisible (
Divisible (..), divided,
) where


import Control.Applicative (liftA2)
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


class (Contravariant m)=> Divisible m where
    divide :: (a -> (b, c)) -> m b -> m c -> m a
    conquer :: m a

    default divide ::
            (gr ~ G.Rep1 m, G.Generic1 m, Divisible gr)=>
            (a -> (b, c)) -> m b -> m c -> m a
    divide = gdivide

    default conquer ::
        (gr ~ G.Rep1 m, G.Generic1 m, Divisible gr)=>
        m a
    conquer = gconquer

divided :: (Divisible m)=> m a -> m b -> m (a, b)
divided = divide id


instance (Monoid r)=> Divisible (Op r) where
    conquer = Op mempty
    {-# INLINE conquer #-}
    divide f (Op g) (Op h) = Op ((\ (x, y) -> g x <> h y) . f)
    {-# INLINABLE divide #-}

instance (Applicative m, Divisible n)=> Divisible (Compose m n)


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Divisible G.U1 where
    conquer = G.U1
    divide _ _ _ = G.U1

instance (Divisible m)=> Divisible (G.Rec1 m) where
    conquer = G.Rec1 conquer
    {-# INLINE conquer #-}
    divide = divideRec1
      where
        divideRec1 ::
            forall a b c.
            (a -> (b, c)) -> G.Rec1 m b -> G.Rec1 m c -> G.Rec1 m a
        divideRec1 = coerce (divide :: (a -> (b, c)) -> m b -> m c -> m a)
        {-# INLINE divideRec1 #-}
    {-# INLINE divide #-}

instance (Divisible m)=> Divisible (G.M1 i info m) where
    conquer = G.M1 conquer
    {-# INLINE conquer #-}
    divide = divideM1
      where
        divideM1 ::
            forall a b c.
            (a -> (b, c)) -> G.M1 i info m b -> G.M1 i info m c -> G.M1 i info m a
        divideM1 = coerce (divide :: (a -> (b, c)) -> m b -> m c -> m a)
        {-# INLINE divideM1 #-}
    {-# INLINE divide #-}

instance (Monoid c)=> Divisible (G.K1 i c) where
    conquer = G.K1 mempty
    {-# INLINE conquer #-}
    divide _ = coerce ((<>) :: c -> c -> c)
    {-# INLINE divide #-}

instance (Divisible m, Divisible n)=> Divisible ((G.:*:) m n) where
    conquer = conquer G.:*: conquer
    {-# INLINABLE conquer #-}
    divide f (mx G.:*: nx) (my G.:*: ny) =
        divide f mx my G.:*: divide f nx ny
    {-# INLINABLE divide #-}

-- This creates a dependency on Apply:
instance (Applicative m, Divisible n)=> Divisible ((G.:.:) m n) where
    conquer = G.Comp1 (pure conquer)
    {-# INLINE conquer #-}
    divide = divideComp1
      where
        divideComp1 :: forall a b c. (a -> (b, c)) -> (G.:.:) m n b -> (G.:.:) m n c -> (G.:.:) m n a
        divideComp1 f = coerce (liftA2 (divide f) :: m (n b) -> m (n c) -> m (n a))
        {-# INLINE divideComp1 #-}
    {-# INLINE divide #-}

gdivide ::
        (gr ~ G.Rep1 m, G.Generic1 m, Divisible gr)=>
        (a -> (b, c)) -> m b -> m c -> m a
gdivide f mx my = G.to1 (divide f (G.from1 mx) (G.from1 my))
{-# INLINE gdivide #-}

gconquer ::
    (gr ~ G.Rep1 m, G.Generic1 m, Divisible gr)=>
    m a
gconquer = G.to1 conquer
{-# INLINE gconquer #-}

#endif

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 781
#else
coerce = unsafeCoerce
#endif
