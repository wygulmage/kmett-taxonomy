{-# LANGUAGE FunctionalDependencies
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , EmptyCase
           , DefaultSignatures
           , BangPatterns
  #-}


module Data.Foldable.WithIndex where

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Data.Functor.Const
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid
import Data.Proxy
import Data.Void
import qualified GHC.Generics as G

class FoldableWithIndex i m | m -> i where
    ifoldMap :: (Monoid b)=> (i -> a -> b) -> m a -> b

    ifoldMap' :: (Monoid b)=> (i -> a -> b) -> m a -> b
    ifoldMap' f = ifoldl' (\ i z x -> z <> f i x) mempty

    default ifoldMap :: (G.Generic1 m, FoldableWithIndex i (G.Rep1 m), Monoid b)=> (i -> a -> b) -> m a -> b
    ifoldMap = gifoldMap


ifoldr :: (FoldableWithIndex i m)=> (i -> a -> b -> b) -> b -> m a -> b
ifoldr f z mx = appEndo (ifoldMap (\ i x -> Endo (f i x)) mx) z

ifoldl' :: (FoldableWithIndex i m)=> (i -> b -> a -> b) -> b -> m a -> b
ifoldl' f z0 mx = ifoldr f' id mx z0
  where f' i x k z = k $! f i z x

-- itraverse_ f = ifoldr (\ i x z -> f i x *> z) (pure ())
ifor_ :: (FoldableWithIndex i m, Applicative n)=> m a -> (i -> a -> n ()) -> n ()
ifor_ = \ mx f -> unThen_ (ifoldMap (\ i x -> Then_ (f i x)) mx)
{-# INLINABLE ifor_ #-}

newtype Then_ m = Then_ (m ())
unThen_ :: Then_ m -> m ()
unThen_ (Then_ m) = m
instance (Applicative m)=> Semigroup (Then_ m) where
    Then_ m1 <> Then_ m2 = Then_ (m1 *> m2)
    {-# INLINE (<>) #-}


instance (Applicative m)=> Monoid (Then_ m) where
    mempty = Then_ (pure ())
    {-# INLINE mempty #-}


instance FoldableWithIndex Void Proxy

instance FoldableWithIndex Void (Const c) where ifoldMap _ _ = mempty

instance FoldableWithIndex () Identity

instance FoldableWithIndex () Maybe where ifoldMap f = maybe mempty (f ())

instance FoldableWithIndex Int [] where
    ifoldMap = ifoldMapListFrom 0
    {-# INLINABLE ifoldMap #-}

ifoldMapListFrom i0 f = loop i0
  where
    loop !i xs = case xs of
        x : xs' -> f i x <> loop (i + 1) xs'
        [] -> mempty

instance FoldableWithIndex Int NonEmpty where
    ifoldMap f (x :| xs) = f 0 x <> ifoldMapListFrom 1 f xs
    {-# INLINABLE ifoldMap #-}

instance FoldableWithIndex () ((,) c) where
    ifoldMap f = f () . snd
    {-# INLINE ifoldMap #-}

instance FoldableWithIndex () (Either c) where
    ifoldMap f = either mempty (f ())
    {-# INLINE ifoldMap #-}


instance FoldableWithIndex Int Seq.Seq where
    ifoldMap = Seq.foldMapWithIndex
    {-# INLINE ifoldMap #-}

instance FoldableWithIndex Int IntMap.IntMap where
    ifoldMap = IntMap.foldMapWithKey
    {-# INLINE ifoldMap #-}

instance FoldableWithIndex i (Map.Map i) where
    ifoldMap = Map.foldMapWithKey
    {-# INLINE ifoldMap #-}


--- GHC Generics ---
instance FoldableWithIndex Void G.V1 where
    ifoldMap _ v1 = case v1 of {}
    ifoldMap' _ v1 = case v1 of {}

instance FoldableWithIndex Void G.U1 where
    ifoldMap _ _ = mempty
    ifoldMap' _ _ = mempty

instance FoldableWithIndex Void (G.K1 i c) where
    ifoldMap _ _ = mempty
    ifoldMap' _ _ = mempty

instance FoldableWithIndex () G.Par1 where
    ifoldMap f (G.Par1 x) = f () x
    ifoldMap' f (G.Par1 x) = f () x

instance (FoldableWithIndex i m)=> FoldableWithIndex i (G.Rec1 m) where
    ifoldMap f (G.Rec1 mx) = ifoldMap f mx
    {-# INLINE ifoldMap #-}
    ifoldMap' f (G.Rec1 mx) = ifoldMap' f mx
    {-# INLINE ifoldMap' #-}

instance (FoldableWithIndex i m)=> FoldableWithIndex i (G.M1 j meta m) where
    ifoldMap f (G.M1 mx) = ifoldMap f mx
    {-# INLINE ifoldMap #-}
    ifoldMap' f (G.M1 mx) = ifoldMap' f mx
    {-# INLINE ifoldMap' #-}

gifoldMap :: (G.Generic1 m, FoldableWithIndex i (G.Rep1 m), Monoid b)=> (i -> a -> b) -> m a -> b
gifoldMap f = ifoldMap f . G.from1
{-# INLINE gifoldMap #-}
