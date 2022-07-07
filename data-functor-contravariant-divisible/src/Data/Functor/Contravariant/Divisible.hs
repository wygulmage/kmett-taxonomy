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
Divisible (..),
) where


import Data.Functor.Apply -- for Compose instance
import Data.Functor.Contravariant.Divise

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


class (Divise m)=> Divisible m where
    conquer :: m a

    default conquer ::
        (gr ~ G.Rep1 m, G.Generic1 m, Divisible gr)=>
        m a
    conquer = gconquer

divide :: (Divisible m)=> (a -> (b, c)) -> m b -> m c -> m a
divide = divise
{-# INLINE divide #-}

instance (Monoid r)=> Divisible (Op r) where
    conquer = Op mempty
    {-# INLINE conquer #-}

instance (Apply m, Applicative m, Divisible n)=> Divisible (Compose m n)


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Divisible G.U1 where conquer = G.U1

instance (Divisible m)=> Divisible (G.Rec1 m) where
    conquer = G.Rec1 conquer
    {-# INLINE conquer #-}

instance (Divisible m)=> Divisible (G.M1 i info m) where
    conquer = G.M1 conquer
    {-# INLINE conquer #-}

instance (Monoid c)=> Divisible (G.K1 i c) where
    conquer = G.K1 mempty
    {-# INLINE conquer #-}

instance (Divisible m, Divisible n)=> Divisible ((G.:*:) m n) where
    conquer = conquer G.:*: conquer
    {-# INLINABLE conquer #-}

-- This creates a dependency on Apply:
instance (Apply m, Applicative m, Divisible n)=> Divisible ((G.:.:) m n) where
    conquer = G.Comp1 (pure conquer)
    {-# INLINE conquer #-}

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
