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


module Data.Functor.Contravariant.Decidable where

import Data.Functor.Contravariant.Divisible

import Control.Applicative (liftA2)
import Data.Functor.Contravariant
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Void

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
import qualified GHC.Generics as G
#endif
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 781
import Data.Coerce (coerce)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif


class (Contravariant m)=> Decidable m where
    lose :: (a -> Void) -> m a
    choose :: (a -> Either b c) -> m b -> m c -> m a

    default choose ::
        (gr ~ G.Rep1 m, G.Generic1 m, Decidable gr)=>
        (a -> Either b c) -> m b -> m c -> m a
    choose = gchoose

    default lose ::
        (gr ~ G.Rep1 m, G.Generic1 m, Decidable gr)=>
        (a -> Void) -> m a
    lose = glose

chosen :: (Decidable m)=>  m b -> m c -> m (Either b c)
chosen = choose id
{-# INLINE chosen #-}

lost :: (Decidable m)=> m Void
lost = lose id
{-# INLINE lost #-}

instance (Monoid r)=> Decidable (Op r) where
    lose f = Op (absurd . f)
    choose f (Op g) (Op h) = Op (either g h . f)

instance (Applicative m, Decidable n)=> Decidable (Compose m n)


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Decidable G.U1 where
    choose _ _ _ = G.U1
    lose _ = G.U1

instance (Decidable m)=> Decidable (G.Rec1 m) where
    choose = chooseRec1
      where
        chooseRec1 ::
            forall a b c.
            (a -> Either b c) -> G.Rec1 m b -> G.Rec1 m c -> G.Rec1 m a
        chooseRec1 = coerce (choose :: (a -> Either b c) -> m b -> m c -> m a)
        {-# INLINE chooseRec1 #-}
    {-# INLINE choose #-}

    lose f = G.Rec1 (lose f)

instance (Decidable m)=> Decidable (G.M1 i info m) where
    choose = chooseM1
      where
        chooseM1 ::
            forall a b c.
            (a -> Either b c) -> G.M1 i info m b -> G.M1 i info m c -> G.M1 i info m a
        chooseM1 = coerce (choose :: (a -> Either b c) -> m b -> m c -> m a)
        {-# INLINE chooseM1 #-}
    {-# INLINE choose #-}
    lose f = G.M1 (lose f)


instance (Monoid c)=> Decidable (G.K1 i c) where
    choose _ = coerce ((<>) :: c -> c -> c)
    {-# INLINE choose #-}
    lose _ = G.K1 mempty

instance (Decidable m, Decidable n)=> Decidable ((G.:*:) m n) where
    choose f (mx G.:*: nx) (my G.:*: ny) =
        choose f mx my G.:*: choose f nx ny
    {-# INLINABLE choose #-}
    lose f = lose f G.:*: lose f

-- This creates a dependency on Apply:
instance (Applicative m, Decidable n)=> Decidable ((G.:.:) m n) where
    choose = chooseComp1
      where
        chooseComp1 :: forall a b c. (a -> Either b c) -> (G.:.:) m n b -> (G.:.:) m n c -> (G.:.:) m n a
        chooseComp1 f = coerce (liftA2 (choose f) :: m (n b) -> m (n c) -> m (n a))
        {-# INLINE chooseComp1 #-}
    {-# INLINE choose #-}
    lose f = G.Comp1 (pure (lose f))

gchoose ::
    (gr ~ G.Rep1 m, G.Generic1 m, Decidable gr)=>
    (a -> Either b c) -> m b -> m c -> m a
gchoose f mx my = G.to1 (choose f (G.from1 mx) (G.from1 my))
{-# INLINE gchoose #-}

glose ::
    (gr ~ G.Rep1 m, G.Generic1 m, Decidable gr)=>
    (a -> Void) -> m a
glose f = G.to1 (lose f)
{-# INLINE glose #-}

#endif

#if !(__GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 781)
coerce = unsafeCoerce
#endif
