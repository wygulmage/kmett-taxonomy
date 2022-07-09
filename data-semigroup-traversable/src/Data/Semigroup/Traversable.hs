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

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
import qualified GHC.Generics as G
#endif


class (Foldable1 m, Traversable m)=> Traversable1 m where
    traverse1 :: (Apply n)=> (a -> n b) -> m a -> n (m b)


instance Traversable1 Identity where
    traverse1 f (Identity x) = fmap Identity (f x)


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
instance Traversable1 G.V1 where
    traverse1 _ v1 = case v1 of {}

instance Traversable1 G.Par1 where
    traverse1 f (G.Par1 x) = fmap G.Par1 (f x)

instance (Traversable1 m)=> Traversable1 (G.Rec1 m) where
    traverse1 f (G.Rec1 mx) = fmap G.Rec1 (traverse1 f mx)

instance (Traversable1 m)=> Traversable1 (G.M1 i info m) where
    traverse1 f (G.M1 mx) = fmap G.M1 (traverse1 f mx)

instance (Traversable1 m, Traversable1 n)=> Traversable1 ((G.:*:) m n) where
    traverse1 f (mx G.:*: nx) = liftF2 (G.:*:) (traverse1 f mx) (traverse1 f nx)

instance (Traversable1 m, Traversable1 n)=> Traversable1 ((G.:+:) m n) where
    traverse1 f mnx = case mnx of
        G.L1 mx -> fmap G.L1 (traverse1 f mx)
        G.R1 nx -> fmap G.R1 (traverse1 f nx)

instance (Traversable1 m, Traversable1 n)=> Traversable1 ((G.:.:) m n) where
    traverse1 f (G.Comp1 mnx) = fmap G.Comp1 (traverse1 (traverse1 f) mnx)
#endif
