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


module Data.Functor.Apply (
Apply (..),
) where


import Data.Tagged

import Control.Applicative (liftA2)
import Control.Monad (liftM2)
import Data.Functor.Const
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Proxy

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
import qualified GHC.Generics as G
#endif
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 781
import Data.Coerce (coerce)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif

class (Functor m)=> Apply m where
    liftF2 :: (a -> b -> c) -> m a -> m b -> m c
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
    default liftF2 ::
        (gr ~ G.Rep1 m, G.Generic1 m, Apply gr)=>
        (a -> b -> c) -> m a -> m b -> m c
    liftF2 = gliftF2

#endif

instance Apply IO where liftF2 = liftM2

instance Apply Identity

instance Apply Proxy

instance Apply Maybe where liftF2 = liftM2

instance Apply [] where
    liftF2 = liftA2
    {-# INLINE liftF2 #-}

instance Apply NonEmpty where liftF2 = liftA2

instance Apply (Either c) where liftF2 = liftM2

instance (Semigroup c)=> Apply ((,) c)

instance Apply (Tagged t) where liftF2 = coerce


--- Containers ---
instance (Ord i)=> Apply (Map.Map i) where
    liftF2 = Map.intersectionWith
    {-# INLINE liftF2 #-}

instance Apply IntMap.IntMap where
    liftF2 = IntMap.intersectionWith
    {-# INLINE liftF2 #-}

instance Apply Seq.Seq where
    liftF2 = liftA2
    {-# INLINE liftF2 #-}


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Apply G.V1 where liftF2 _ v1 v2 = v1 `seq` case v2 of {}

instance Apply G.U1 where liftF2 _ _ _ = G.U1

instance Apply G.Par1 where liftF2 = coerce

instance (Apply m)=> Apply (G.Rec1 m) where
    liftF2 = liftRec12
      where
        liftRec12 ::
            forall a b c.
            (a -> b -> c) -> G.Rec1 m a -> G.Rec1 m b -> G.Rec1 m c
        liftRec12 = coerce (liftF2 :: (a -> b -> c) -> m a -> m b -> m c)

instance (Apply m)=> Apply (G.M1 i info m) where
    liftF2 = liftM12
      where
        liftM12 ::
            forall a b c.
            (a -> b -> c) -> G.M1 i info m a -> G.M1 i info m b -> G.M1 i info m c
        liftM12 = coerce (liftF2 :: (a -> b -> c) -> m a -> m b -> m c)

instance (Semigroup c)=> Apply (G.K1 i c) where
    liftF2 _ = coerce ((<>) :: c -> c -> c)
    {-# INLINE liftF2 #-}

instance (Apply m, Apply n)=> Apply ((G.:*:) m n) where
    liftF2 f (mx G.:*: nx) (my G.:*: ny) =
        liftF2 f mx my G.:*: liftF2 f nx ny
    {-# INLINABLE liftF2 #-}

instance (Apply m, Apply n)=> Apply ((G.:.:) m n) where
    liftF2 = liftComp12
      where
        liftComp12 :: forall a b c. (a -> b -> c) -> (G.:.:) m n a -> (G.:.:) m n b -> (G.:.:) m n c
        liftComp12 f = coerce (liftF2 (liftF2 f) :: m (n a) -> m (n b) -> m (n c))
        {-# INLINE liftComp12 #-}
    {-# INLINE liftF2 #-}

gliftF2 ::
    (gr ~ G.Rep1 m, G.Generic1 m, Apply gr)=>
    (a -> b -> c) -> m a -> m b -> m c
gliftF2 f mx my =
    G.to1 (liftF2 f (G.from1 mx) (G.from1 my))
{-# INLINE gliftF2 #-}

#endif

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 781
#else
coerce = unsafeCoerce
#endif
