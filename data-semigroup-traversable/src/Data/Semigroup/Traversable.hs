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



module Data.Semigroup.Traversable where

import Data.Functor.Apply
import Data.Semigroup.Foldable
import Data.Tagged

import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty((:|)))

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
import qualified GHC.Generics as G
#endif

-- Note: Really I want the method of Traversable1 to be traverseMap1 :: (m b -> r) -> (a -> n b) -> m a -> n r, with traverse1 defined as traverseMap id.

class (Foldable1 m, Traversable m)=> Traversable1 m where
    traverse1 :: (Apply n)=> (a -> n b) -> m a -> n (m b)
    default traverse1 ::
        (G.Generic1 m, gr ~ G.Rep1 m, GTraversable1 gr, Apply n)=>
        (a -> n b) -> m a -> n (m b)
    traverse1 = gtraverse1


instance Traversable1 Identity where
    traverse1 f (Identity x) = fmap Identity (f x)

instance Traversable1 NonEmpty where
    traverse1 f (x :| xs) = go x xs
      where
        go y (y' : ys) = liftF2 (:|) (f y) (loop y' ys)
        go y [] = fmap (:| []) (f y)
        loop y (y' : ys) = liftF2 (:) (f y) (loop y' ys)
        loop y [] = fmap (: []) (f y)

instance Traversable1 ((,) c) where
    traverse1 f (u, x) = (,) u `fmap` f x
    {-# INLINABLE traverse1 #-}


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
class GTraversable1 m where
    gtraverseMap1 :: (Apply n)=> (m b -> r) -> (a -> n b) -> m a -> n r

instance Traversable1 G.V1
instance GTraversable1 G.V1 where
    gtraverseMap1 _ _ v1 = case v1 of {}

instance Traversable1 G.Par1
instance GTraversable1 G.Par1 where
    gtraverseMap1 g f (G.Par1 x) = fmap (g . G.Par1) (f x)
    {-# INLINABLE gtraverseMap1 #-}

instance (Traversable1 m)=> Traversable1 (G.Rec1 m)
instance (Traversable1 m)=> GTraversable1 (G.Rec1 m) where
    gtraverseMap1 g f (G.Rec1 mx) = fmap (g . G.Rec1) (traverse1 f mx)
    {-# INLINABLE gtraverseMap1 #-}

instance (Traversable1 m)=> Traversable1 (G.M1 i info m)
instance (Traversable1 m)=> GTraversable1 (G.M1 i info m) where
    gtraverseMap1 g f (G.M1 mx) = fmap (g . G.M1) (traverse1 f mx)
    {-# INLINABLE gtraverseMap1 #-}

instance (Traversable1 m, Traversable1 n)=> Traversable1 ((G.:*:) m n)
instance (Traversable1 m, Traversable1 n)=> GTraversable1 ((G.:*:) m n) where
    gtraverseMap1 g f (mx G.:*: nx) =
        liftF2 (\ x y -> g (x G.:*: y)) (traverse1 f mx) (traverse1 f nx)
    {-# INLINABLE gtraverseMap1 #-}

instance (Traversable1 m, Traversable1 n)=> Traversable1 ((G.:+:) m n)
instance (Traversable1 m, Traversable1 n)=> GTraversable1 ((G.:+:) m n) where
    gtraverseMap1 g f mnx = case mnx of
        G.L1 mx -> fmap (g . G.L1) (traverse1 f mx)
        G.R1 nx -> fmap (g . G.R1) (traverse1 f nx)
    {-# INLINABLE gtraverseMap1 #-}

instance (Traversable1 m, Traversable1 n)=> Traversable1 ((G.:.:) m n)
instance (Traversable1 m, Traversable1 n)=> GTraversable1 ((G.:.:) m n) where
    gtraverseMap1 g f (G.Comp1 mnx) =
        fmap (g . G.Comp1) (traverse1 (traverse1 f) mnx)
    {-# INLINABLE gtraverseMap1 #-}

gtraverse1 ::
    (G.Generic1 m, gr ~ G.Rep1 m, GTraversable1 gr, Apply n)=>
    (a -> n b) -> m a -> n (m b)
gtraverse1 f = gtraverseMap1 G.to1 f . G.from1
{-# INLINE gtraverse1 #-}
#endif
