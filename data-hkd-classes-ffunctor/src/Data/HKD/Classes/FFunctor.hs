{-# LANGUAGE DefaultSignatures
           , EmptyCase
           , KindSignatures
           , PolyKinds
           , RankNTypes
           , ScopedTypeVariables
           , TypeFamilies
  #-}

module Data.HKD.Classes.FFunctor (FFunctor (..)) where


import Data.Functor.Compose
import Data.Functor.Const
import qualified Data.Functor.Product as Functor
import qualified Data.Functor.Sum as Functor
import Data.Proxy

import Data.Coerce (Coercible, coerce)
import GHC.Generics


class FFunctor (t :: (k -> *) -> *) where
{-^ @FFunctor@s are higher-kinded 'Functors'. You can think of them as mapping natural transformations rather than functions; that is, mapping functors to functors (although 'ffmap' does not actually require that its objects be members of the 'Functor' class).
-}
    ffmap :: (forall x. m x -> n x) -> t m -> t n

    default ffmap :: (Generic1 t, rt ~ Rep1 t, FFunctor rt)=> (forall x. m x -> n x) -> t m -> t n
    ffmap f = to1 . ffmap f . from1


--- base instances ---

instance FFunctor Proxy
instance FFunctor (Const c)
instance (Functor m, FFunctor t)=> FFunctor (Compose m t)
instance (FFunctor t1, FFunctor t2)=> FFunctor (Functor.Product t1 t2)
instance (FFunctor t1, FFunctor t2)=> FFunctor (Functor.Sum t1 t2)


--- GHC.Generics ---

instance FFunctor V1 where ffmap _ v1 = case v1 of {}
instance FFunctor U1 where ffmap _ _ = U1
instance FFunctor (K1 p c) where ffmap _ = coerce

instance (FFunctor t)=> FFunctor (Rec1 t) where
    ffmap f = Rec1 #. ffmap f .# unRec1
    {-# INLINE ffmap #-}

instance (FFunctor t)=> FFunctor (M1 i info t) where
    ffmap f = M1 #. ffmap f .# unM1
    {-# INLINE ffmap #-}

instance (Functor l, FFunctor t)=> FFunctor ((:.:) l t) where
    ffmap f = Comp1 #. fmap (ffmap f) .# unComp1
    {-# INLINE ffmap #-}

instance (FFunctor t1, FFunctor t2)=> FFunctor ((:*:) t1 t2) where
    ffmap f (t1 :*: t2) = ffmap f t1 :*: ffmap f t2
    {-# INLINE ffmap #-}

instance (FFunctor t1, FFunctor t2)=> FFunctor ((:+:) t1 t2) where
    ffmap f (L1 t1) = L1 (ffmap f t1)
    ffmap f (R1 t2) = R1 (ffmap f t2)
    {-# INLINE ffmap #-}


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
