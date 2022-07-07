{-# LANGUAGE TypeFamilies
           , DefaultSignatures
           , GADTs
           , EmptyCase
           , ScopedTypeVariables
           , UndecidableInstances -- for generic type family
           , Trustworthy -- lol
  #-}


module Data.Functor.Contravariant.Rep (
Representable (..), contramapRep, diviseRep, conquerRep, decideRep, concludedRep,
) where


import Data.Functor.Contravariant.Conclude

import Data.Functor.Compose (Compose)
import Data.Functor.Contravariant
import Data.Monoid (All(..))
import Data.Proxy
import Data.Void

import qualified GHC.Generics as G
import Data.Coerce (coerce)


class (Conclude m)=> Representable m where
    type Rep m
    tabulate :: (a -> Rep m) -> m a
    index :: m a -> a -> Rep m

    type Rep m = Rep (G.Rep1 m)
    default tabulate ::
        (gr ~ G.Rep1 m, Rep gr ~ Rep m, G.Generic1 m, Representable gr)=>
        (a -> Rep m) -> m a
    tabulate = gtabulate

    default index ::
        (gr ~ G.Rep1 m, Rep gr ~ Rep m, G.Generic1 m, Representable gr)=>
        m a -> (a -> Rep m)
    index = gindex

contramapRep :: (Representable m)=> (a -> b) -> m b -> m a
contramapRep f = tabulate . (. f) . index
{-# INLINE contramapRep #-}

diviseRep ::
    (Semigroup r, r ~ Rep m, Representable m)=> (a -> (b, c)) -> m b -> m c -> m a
diviseRep f mx my = tabulate ((\ (x, y) -> index mx x <> index my y) . f)
{-# INLINE diviseRep #-}

conquerRep ::
    (Monoid r, r ~ Rep m, Representable m)=> m a
conquerRep = tabulate mempty
{-# INLINE conquerRep #-}

decideRep :: (Representable m)=> (a -> Either b c) -> m b -> m c -> m a
decideRep f mx my = tabulate (either (index mx) (index my) . f)

concludedRep :: (Representable m)=> m Void
concludedRep = tabulate absurd
{-# INLINE concludedRep #-}


instance Representable (Op r) where
    type Rep (Op r) = r
    tabulate = Op
    index = getOp

instance Representable Predicate where
    type Rep Predicate = All -- ^ Results are combined with '&&'. Adjunctions lib uses Bool.
    tabulate = coerce
    index = coerce

instance Representable Proxy


--- Generics ---
instance Representable G.U1 where
    type Rep G.U1 = ()
    tabulate _ = G.U1
    index _ = mempty

instance (Representable m)=> Representable (G.Rec1 m) where
    type Rep (G.Rec1 m) = Rep m
    tabulate f = G.Rec1 (tabulate f)
    index (G.Rec1 mx) = index mx

instance (Representable m)=> Representable (G.M1 i info m) where
    type Rep (G.M1 i info m) = Rep m
    tabulate f = G.M1 (tabulate f)
    index (G.M1 mx) = index mx

instance (Representable m, Representable n)=> Representable ((G.:*:) m n) where
    type Rep ((G.:*:) m n) = (Rep m, Rep n)
    tabulate f = tabulate (fst . f) G.:*: tabulate (snd . f)
    {-# INLINABLE tabulate #-}
    index (mx G.:*: nx) x = (index mx x, index nx x)
    {-# INLINABLE index #-}

gtabulate :: (gr ~ G.Rep1 m, G.Generic1 m, Representable gr)=> (a -> Rep gr) -> m a
gtabulate = G.to1 . tabulate
{-# INLINE gtabulate #-}

gindex :: (gr ~ G.Rep1 m, G.Generic1 m, Representable gr)=> m a -> (a -> Rep gr)
gindex = index . G.from1
{-# INLINE gindex #-}
