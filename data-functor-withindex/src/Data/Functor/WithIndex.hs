{-# LANGUAGE FunctionalDependencies
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , EmptyCase
           , DefaultSignatures
           , BangPatterns
  #-}

module Data.Functor.WithIndex where


import qualified Data.IntMap as IntMap
import qualified Data.Map.Lazy as Map
import qualified Data.Sequence as Seq

import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Proxy
import Data.Void
import qualified GHC.Generics as G
import qualified GHC.Exts as Exts


class (Functor m)=> FunctorWithIndex i m | m -> i where
    imap :: (i -> a -> b) -> m a -> m b

    default imap :: (G.Generic1 m, FunctorWithIndex i (G.Rep1 m))=> (i -> a -> b) -> m a -> m b
    imap = gimap


instance FunctorWithIndex Void Proxy

instance FunctorWithIndex () Identity

instance FunctorWithIndex () Maybe where
    imap f = maybe Nothing (Just . f ())

instance FunctorWithIndex Int [] where
    imap = imapListFrom 0
    {-# INLINE imap #-}

imapListFrom i0 f = \ xs -> Exts.build $ \ c n ->
  let
    loop !i ys = case ys of
        y : ys' -> f i y `c` loop (i + 1) ys'
        [] -> n
  in loop i0 xs
{-# INLINE [~0] imapListFrom #-}

instance FunctorWithIndex Int NonEmpty where
    imap f (x :| xs) = f 0 x :| imapListFrom 1 f xs
    {-# INLINE imap #-}

instance FunctorWithIndex Int IntMap.IntMap where
    imap = IntMap.mapWithKey
    {-# INLINE imap #-}

instance FunctorWithIndex i (Map.Map i) where
    imap = Map.mapWithKey
    {-# INLINE imap #-}

instance FunctorWithIndex Int Seq.Seq where
    imap = Seq.mapWithIndex
    {-# INLINE imap #-}


--- Generics ---
-- The V1 instance is illegal because of the functional dependency V1 -> i.
-- instance FunctorWithIndex i G.V1 where
--     imap _ v1 = case v1 of {}

instance FunctorWithIndex Void G.U1 where
    imap _ _ = G.U1

instance FunctorWithIndex Void (G.K1 j c) where
    imap _ (G.K1 x) = G.K1 x

instance FunctorWithIndex () G.Par1 where
    imap f (G.Par1 x) = G.Par1 (f () x)

instance (FunctorWithIndex i m)=> FunctorWithIndex i (G.Rec1 m) where
    imap f (G.Rec1 mx) = G.Rec1 (imap f mx)
    {-# INLINE imap #-}

instance (FunctorWithIndex i m)=> FunctorWithIndex i (G.M1 j meta m) where
    imap f (G.M1 mx) = G.M1 (imap f mx)
    {-# INLINE imap #-}

instance (FunctorWithIndex i m, FunctorWithIndex j n)=> FunctorWithIndex (Either i j) ((G.:*:) m n) where
    imap f (mx G.:*: nx) = imap (\ i x -> f (Left i) x) mx G.:*: imap (\ j x -> f (Right j) x) nx
    {-# INLINABLE imap #-}

instance (FunctorWithIndex i m, FunctorWithIndex j n)=> FunctorWithIndex (Either i j) ((G.:+:) m n) where
    imap f mnx = case mnx of
        G.L1 mx -> G.L1 (imap (\ i x -> f (Left i) x) mx)
        G.R1 nx -> G.R1 (imap (\ j x -> f (Right j) x) nx)
    {-# INLINABLE imap #-}

instance (FunctorWithIndex i m, FunctorWithIndex j n)=> FunctorWithIndex (i, j) ((G.:.:) m n) where
    imap f (G.Comp1 mnx) = G.Comp1 (imap (\ i nx -> imap (\ j x -> f (i, j) x) nx) mnx)
    {-# INLINABLE imap #-}

gimap :: (G.Generic1 m, FunctorWithIndex i (G.Rep1 m))=> (i -> a -> b) -> m a -> m b
gimap f = G.to1 . imap f . G.from1
{-# INLINE gimap #-}
