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


module Data.Semigroup.Foldable where

import Data.Foldable
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Exts (augment)
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
import qualified GHC.Generics as G
#endif
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 781
import Data.Coerce (coerce)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif


class (Foldable m)=> Foldable1 m where
    {-# MINIMAL foldMap1 | toNonEmpty #-}
    fold1 :: (Semigroup a)=> m a -> a
    fold1 = foldMap1 id
    foldMap1 :: (Semigroup b)=> (a -> b) -> m a -> b
    foldMap1 f = f `seq` foldMap1 f . toNonEmpty
    toNonEmpty :: m a -> NonEmpty a
    toNonEmpty = foldMap1 (:| [])


instance Foldable1 Identity where
    foldMap1 = gfoldMap1

instance Foldable1 NonEmpty where
    toNonEmpty = id
    foldMap1 f (x :| xs) = f `seq` loop x xs
      where
        loop y (y' : ys') = f y <> loop y' ys'
        loop y [] = f y


appendNEList :: NonEmpty a -> [a] -> NonEmpty a
appendNEList (x :| xs) xs' = x :| augment (\ c n -> foldr c n xs) xs'
{-# INLINE [~0] appendNEList #-}


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
absurd1 :: G.V1 a -> b
absurd1 v1 = case v1 of {}

instance Foldable1 G.V1 where
    fold1 = absurd1
    foldMap1 _ = absurd1
    toNonEmpty = absurd1

instance Foldable1 G.Par1 where
    fold1 = coerce
    foldMap1 = coerce
    toNonEmpty (G.Par1 x) = x :| []

instance (Foldable1 m)=> Foldable1 (G.Rec1 m) where
    fold1 (G.Rec1 mx) = fold1 mx
    foldMap1 f (G.Rec1 mx) = foldMap1 f mx
    toNonEmpty (G.Rec1 mx) = toNonEmpty mx

instance (Foldable1 m)=> Foldable1 (G.M1 i info m) where
    fold1 (G.M1 mx) = fold1 mx
    foldMap1 f (G.M1 mx) = foldMap1 f mx
    toNonEmpty (G.M1 mx) = toNonEmpty mx

instance (Foldable1 m, Foldable n)=> Foldable1 ((G.:*:) m n) where
    fold1 (mx G.:*: nx) = fold1 (appendNEList (toNonEmpty mx) (toList nx))
    foldMap1 f (mx G.:*: nx) = foldMap1 f (appendNEList (toNonEmpty mx) (toList nx))
    toNonEmpty (mx G.:*: nx) = appendNEList (toNonEmpty mx) (toList nx)

eitherM :: (m a -> r) -> (n a -> r) -> (G.:+:) m n a -> r
eitherM f g mnx = case mnx of
    G.L1 mx -> f mx
    G.R1 nx -> g nx

instance (Foldable1 m, Foldable1 n)=> Foldable1 ((G.:+:) m n) where
    fold1 = eitherM fold1 fold1
    foldMap1 f = eitherM (foldMap1 f) (foldMap1 f)
    toNonEmpty = eitherM toNonEmpty toNonEmpty

instance (Foldable1 m, Foldable1 n)=> Foldable1 ((G.:.:) m n) where
    fold1 (G.Comp1 mnx) = foldMap1 fold1 mnx
    foldMap1 f (G.Comp1 mnx) = foldMap1 (foldMap1 f) mnx
    toNonEmpty (G.Comp1 mnx) = foldMap1 toNonEmpty mnx


gfold1 :: (gr ~ G.Rep1 m, G.Generic1 m, Foldable1 gr, Semigroup a)=> m a -> a
gfold1 = fold1 . G.from1
{-# INLINE gfold1 #-}

gfoldMap1 ::
    (gr ~ G.Rep1 m, G.Generic1 m, Foldable1 gr, Semigroup b)=>
    (a -> b) ->  m a -> b
gfoldMap1 f = foldMap1 f . G.from1
{-# INLINE gfoldMap1 #-}

gtoNonEmpty ::
    (gr ~ G.Rep1 m, G.Generic1 m, Foldable1 gr)=> m a -> NonEmpty a
gtoNonEmpty = toNonEmpty . G.from1
{-# INLINE gtoNonEmpty #-}

#endif

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 781
#else
coerce = unsafeCoerce
#endif
