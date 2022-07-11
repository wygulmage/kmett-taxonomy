{-# LANGUAGE FunctionalDependencies
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , EmptyCase
           , DefaultSignatures
           , BangPatterns
--           , TypeFamilies
  #-}

module Data.Functor.WithIndex where


import qualified Data.IntMap as IntMap
import qualified Data.Map.Lazy as Map
import qualified Data.Sequence as Seq

import Data.Functor.Const
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Proxy
import Data.Void
import qualified GHC.Generics as G
import qualified GHC.Exts as Exts


class (Functor m)=> FunctorWithIndex i m | m -> i where
    imap :: (i -> a -> b) -> m a -> m b

    default imap :: (G.Generic1 m, GFunctorWithIndex i (G.Rep1 m))=> (i -> a -> b) -> m a -> m b
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

instance FunctorWithIndex Void (Const c)

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
class GFunctorWithIndex i m where
    gimap' :: (i -> a -> b) -> m a -> m b

instance GFunctorWithIndex Void G.V1 where -- should be forall i. i, but that won't help with generalized deriving.
    gimap' _ v1 = case v1 of {}

instance GFunctorWithIndex Void G.U1 where gimap' _ _ = G.U1
instance FunctorWithIndex Void G.U1 where imap = gimap'

instance GFunctorWithIndex Void (G.K1 j c) where gimap' _ (G.K1 x) = G.K1 x
instance FunctorWithIndex Void (G.K1 j c) where imap = gimap'

instance GFunctorWithIndex () G.Par1 where gimap' f (G.Par1 x) = G.Par1 (f () x)
instance FunctorWithIndex () G.Par1 where imap = gimap'

instance (GFunctorWithIndex i m)=> GFunctorWithIndex i (G.Rec1 m) where
    gimap' f (G.Rec1 mx) = G.Rec1 (gimap' f mx)
    {-# INLINE gimap' #-}
instance (FunctorWithIndex i m)=> FunctorWithIndex i (G.Rec1 m) where
    imap f (G.Rec1 mx) = G.Rec1 (imap f mx)
    {-# INLINE imap #-}

instance (FunctorWithIndex i m)=> GFunctorWithIndex i (G.M1 j meta m) where
    gimap' f (G.M1 mx) = G.M1 (imap f mx)
    {-# INLINE gimap' #-}
instance (FunctorWithIndex i m)=> FunctorWithIndex i (G.M1 j meta m) where
    imap = gimap'
    {-# INLINE imap #-}

instance (GFunctorWithIndex i m, GFunctorWithIndex j n)=> GFunctorWithIndex (Either i j) ((G.:*:) m n) where
    gimap' f (mx G.:*: nx) = gimap' (\ i x -> f (Left i) x) mx G.:*: gimap' (\ j x -> f (Right j) x) nx
    {-# INLINABLE gimap' #-}
instance (FunctorWithIndex i m, FunctorWithIndex j n)=> FunctorWithIndex (Either i j) ((G.:*:) m n) where
    imap f (mx G.:*: nx) = imap (\ i x -> f (Left i) x) mx G.:*: imap (\ j x -> f (Right j) x) nx
    {-# INLINABLE imap #-}

instance (GFunctorWithIndex i m, GFunctorWithIndex j n)=> GFunctorWithIndex (Either i j) ((G.:+:) m n) where
    gimap' f mnx = case mnx of
        G.L1 mx -> G.L1 (gimap' (\ i x -> f (Left i) x) mx)
        G.R1 nx -> G.R1 (gimap' (\ j x -> f (Right j) x) nx)
    {-# INLINABLE gimap' #-}
instance (FunctorWithIndex i m, FunctorWithIndex j n)=> FunctorWithIndex (Either i j) ((G.:+:) m n) where
    imap f mnx = case mnx of
        G.L1 mx -> G.L1 (imap (\ i x -> f (Left i) x) mx)
        G.R1 nx -> G.R1 (imap (\ j x -> f (Right j) x) nx)
    {-# INLINABLE imap #-}

instance (GFunctorWithIndex i m, GFunctorWithIndex j n)=> GFunctorWithIndex (i, j) ((G.:.:) m n) where
    gimap' f (G.Comp1 mnx) = G.Comp1 (gimap' (\ i nx -> gimap' (\ j x -> f (i, j) x) nx) mnx)
    {-# INLINABLE gimap' #-}
instance (FunctorWithIndex i m, FunctorWithIndex j n)=> FunctorWithIndex (i, j) ((G.:.:) m n) where
    imap f (G.Comp1 mnx) = G.Comp1 (imap (\ i nx -> imap (\ j x -> f (i, j) x) nx) mnx)
    {-# INLINABLE imap #-}

-- type family SimplifyIndex i where
--     SimplifyIndex (Either Void i) = i
--     SimplifyIndex (Either i Void) = i
--     SimplifyIndex (Void, i) = Void
--     SimplifyIndex (i, Void) = Void
--     SimplifyIndex ((), i) = i
--     SimplifyIndex (i, ()) = i
--     SimplifyIndex i = i

gimap :: (G.Generic1 m, GFunctorWithIndex i (G.Rep1 m))=> (i -> a -> b) -> m a -> m b
gimap f = G.to1 . gimap' f . G.from1
{-# INLINE gimap #-}
