{-# LANGUAGE FunctionalDependencies
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , EmptyCase
           , DefaultSignatures
           , BangPatterns
  #-}

{- NOTES
The current indexed-traversable package is probably perfect as is (aside, perhaps, from lacking an itraverseMap method). Splitting into 3 packages has very little benefit, and the detriment (that ifoldMapDefault and imapDefault can't be used as default methods) is severe. GHC generics aren't very helpful, since they tend to generate uselessly complex indices.
-}


module Data.Traversable.WithIndex where


import Data.Foldable.WithIndex
import Data.Functor.WithIndex

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Control.Applicative
import Data.Functor.Const
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid
import Data.Proxy
import Data.Void
import qualified GHC.Generics as G


class (FoldableWithIndex i m, FunctorWithIndex i m)=> TraversableWithIndex i m | m -> i where
    itraverse :: (Applicative n)=> (i -> a -> n b) -> m a -> n (m b)

    default itraverse ::
        (G.Generic1 m, GTraversableWithIndex i (G.Rep1 m), Applicative n)=> (i -> a -> n b) -> m a -> n (m b)
    itraverse = gitraverse

instance TraversableWithIndex Void Proxy
instance TraversableWithIndex () Identity
instance TraversableWithIndex () Maybe where
    itraverse f = traverse (f ())
    {-# INLINE itraverse #-}

instance TraversableWithIndex Int [] where
    itraverse = itraverseListFrom 0

itraverseListFrom :: (Applicative m)=> Int -> (Int -> a -> m b) -> [a] -> m [b]
itraverseListFrom i0 f = loop i0
   where
     loop !i xs = case xs of
         x : xs' -> liftA2 (:) (f i x) (loop (i + 1) xs')
         [] -> pure []

instance TraversableWithIndex Int NonEmpty where
    itraverse f (x :| xs) = liftA2 (:|) (f 0 x) (itraverseListFrom 1 f xs)

instance TraversableWithIndex Void (Const c)

instance TraversableWithIndex Int IntMap.IntMap where
    itraverse = IntMap.traverseWithKey
    {-# INLINE itraverse #-}

instance TraversableWithIndex i (Map.Map i) where
    itraverse = Map.traverseWithKey
    {-# INLINE itraverse #-}

--- GHC Generics ---
class GTraversableWithIndex i m where
    gitraverseMap :: (Applicative n)=> (m b -> r) -> (i -> a -> n b) -> m a -> n r

gitraverse :: (G.Generic1 m, GTraversableWithIndex i (G.Rep1 m), Applicative n)=> (i -> a -> n b) -> m a -> n (m b)
gitraverse f = gitraverseMap G.to1 f . G.from1
{-# INLINE gitraverse #-}

instance GTraversableWithIndex i G.V1 where
    gitraverseMap _ _ v = case v of {}

instance GTraversableWithIndex i G.U1 where
    gitraverseMap g _ _ = pure (g G.U1)

instance GTraversableWithIndex i (G.K1 j c) where
    gitraverseMap g _ (G.K1 x) = pure (g (G.K1 x))

instance GTraversableWithIndex () G.Par1 where
    gitraverseMap g f (G.Par1 x) = (g . G.Par1) `fmap` f () x

instance (GTraversableWithIndex i m)=> GTraversableWithIndex i (G.Rec1 m) where
    gitraverseMap g f (G.Rec1 mx) = gitraverseMap (g . G.Rec1) f mx

instance (GTraversableWithIndex i m)=> GTraversableWithIndex i (G.M1 j meta m) where
    gitraverseMap g f (G.M1 mx) = gitraverseMap (g . G.M1) f mx

instance (GTraversableWithIndex i m, GTraversableWithIndex j n)=> GTraversableWithIndex (Either i j) ((G.:+:) m n) where
   gitraverseMap g f mnx = case mnx of
       G.L1 mx -> gitraverseMap (g . G.L1) (\ i x -> f (Left i) x) mx
       G.R1 nx -> gitraverseMap (g . G.R1) (\ i x -> f (Right i) x) nx
