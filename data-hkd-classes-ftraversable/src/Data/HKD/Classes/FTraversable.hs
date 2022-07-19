{-# LANGUAGE DefaultSignatures
           , EmptyCase
           , RankNTypes
           , ScopedTypeVariables
           , TypeFamilies
  #-}

module Data.HKD.Classes.FTraversable (
FTraversable, ftraverseMap, ftraverse, ffmapDefault, ffoldMapDefault,
FFunctor, ffmap,
FFoldable, ffoldMap, ffoldMapGeneric,
) where

import Data.HKD.Classes.FFunctor

import Control.Applicative (liftA2)
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.Functor.Product as Functor
import qualified Data.Functor.Sum as Functor
import Data.Proxy

import Data.Coerce (Coercible, coerce)

import GHC.Generics


class FFoldable t where
    ffoldMap :: (Monoid r)=> (forall x. m x -> r) -> t m -> r

    default ffoldMap :: (FTraversable t, Monoid r)=> (forall x. m x -> r) -> t m -> r
    ffoldMap = ffoldMapDefault


class (FFoldable t, FFunctor t)=> FTraversable t where
    ftraverseMap :: (Applicative o)=> (t n -> r) -> (forall x. m x -> o (n x)) -> t m -> o r

    default ftraverseMap ::
        (Generic1 t, rt ~ Rep1 t, FTraversable rt, Applicative o)=>  (t n -> r) -> (forall x. m x -> o (n x)) -> t m -> o r
    ftraverseMap g f = ftraverseMap (g . to1) f . from1

ftraverse :: (FTraversable t, Applicative o)=> (forall x. m x -> o (n x)) -> t m -> o (t n)
ftraverse = ftraverseMap id

ffoldMapDefault :: (FTraversable t, Monoid r)=> (forall x. m x -> r) -> t m -> r
ffoldMapDefault = \ f ->  getConst #. ftraverse (Const #. f)
{-# INLINE ffoldMapDefault #-}

ffmapDefault :: (FTraversable t)=> (forall x. m x -> n x) -> t m -> t n
ffmapDefault = \ f -> runIdentity #. ftraverse (Identity #. f)
{-# INLINE ffmapDefault #-}


--- Base Instances ---
instance FTraversable Proxy
instance FFoldable Proxy

instance (Traversable t1, FTraversable t2)=> FTraversable (Compose t1 t2)
instance (Foldable t1, FFoldable t2)=> FFoldable (Compose t1 t2) where
    ffoldMap f = foldMap (ffoldMap f) .# getCompose
    {-# INLINE ffoldMap #-}

instance (FTraversable t1, FTraversable t2)=> FTraversable (Functor.Product t1 t2)
instance (FFoldable t1, FFoldable t2)=> FFoldable (Functor.Product t1 t2) where ffoldMap = ffoldMapGeneric

instance (FTraversable t1, FTraversable t2)=> FTraversable (Functor.Sum t1 t2)
instance (FFoldable t1, FFoldable t2)=> FFoldable (Functor.Sum t1 t2) where ffoldMap = ffoldMapGeneric


--- GHC Generics ---

ffoldMapGeneric :: (Generic1 t, rt ~ Rep1 t, FFoldable rt, Monoid r)=> (forall x. m x -> r) -> t m -> r
ffoldMapGeneric = \ f -> ffoldMap f . from1
{-# INLINE ffoldMapGeneric #-}

ffmapGeneric :: (Generic1 t, rt ~ Rep1 t, FFunctor rt)=> (forall x. m x -> n x) -> t m -> t n
ffmapGeneric = \ f -> to1 . ffmap f . from1
{-# INLINE ffmapGeneric #-}

instance FTraversable V1 where
    ftraverseMap _ _ v1 = case v1 of {}
    {-# INLINE ftraverseMap #-}
instance FFoldable V1

instance FTraversable U1 where
    ftraverseMap g _ U1 = pure (g U1)
    {-# INLINE ftraverseMap #-}
instance FFoldable U1

instance FTraversable (K1 i c) where
    ftraverseMap g _ = pure . g .# (K1 . unK1)
    {-# INLINE ftraverseMap #-}
instance FFoldable (K1 i c)

instance (FTraversable t)=> FTraversable (Rec1 t) where
    ftraverseMap g f = ftraverseMap (g .# Rec1) f .# unRec1
    {-# INLINE ftraverseMap #-}

instance (FFoldable t)=> FFoldable (Rec1 t) where
    ffoldMap f = ffoldMap f .# unRec1
    {-# INLINE ffoldMap #-}


instance (FTraversable t)=> FTraversable (M1 i info t) where
    ftraverseMap g f = ftraverseMap (g .# M1) f .# unM1
    {-# INLINE ftraverseMap #-}

instance (FFoldable t)=> FFoldable (M1 i info t) where
    ffoldMap f = ffoldMap f .# unM1
    {-# INLINE ffoldMap #-}

instance (Traversable l, FTraversable t)=> FTraversable ((:.:) l t) where
    ftraverseMap g f = fmap (g .# Comp1) . traverse (ftraverse f) .# unComp1
    {-# INLINE ftraverseMap #-}

instance (Foldable l, FFoldable t)=> FFoldable ((:.:) l t) where
    ffoldMap f = foldMap (ffoldMap f) .# unComp1
    {-# INLINE ffoldMap #-}

instance (FTraversable t1, FTraversable t2)=> FTraversable ((:*:) t1 t2) where
    ftraverseMap g f (t1 :*: t2) = ftraverseMap (g .: (:*:)) f t1 <*> ftraverse f t2
    {-# INLINE ftraverseMap #-}

instance (FFoldable t1, FFoldable t2)=> FFoldable ((:*:) t1 t2) where
    ffoldMap f (t1 :*: t2) = ffoldMap f t1 <> ffoldMap f t2
    {-# INLINE ffoldMap #-}

instance (FTraversable t1, FTraversable t2)=> FTraversable ((:+:) t1 t2) where
    ftraverseMap g f (L1 t1) = ftraverseMap (g . L1) f t1
    ftraverseMap g f (R1 t2) = ftraverseMap (g . R1) f t2
    {-# INLINE ftraverseMap #-}

instance (FFoldable t1, FFoldable t2)=> FFoldable ((:+:) t1 t2) where
    ffoldMap f (L1 t1) = ffoldMap f t1
    ffoldMap f (R1 t2) = ffoldMap f t2
    {-# INLINE ffoldMap #-}


--- Helpers ---

(.:) :: (c -> c') -> (a -> b -> c) -> a -> b -> c'
(.:) f = f `seq` \ g x y -> f (g x y)
{-# INLINE (.:) #-}

infixr 9 .#
( .# ) :: (Coercible b a)=> (b -> c) -> p a b -> a -> c
f .# _ = coerce f
{-# INLINE ( .# ) #-}

infixl 8 #.
( #. ) :: (Coercible c b)=> p b c -> (a -> b) -> a -> c
( #. ) _ = coerce
{-# INLINE ( #. ) #-}
