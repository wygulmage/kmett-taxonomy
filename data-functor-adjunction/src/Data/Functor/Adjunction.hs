{-# LANGUAGE CPP
           , FunctionalDependencies
           , DefaultSignatures
           , EmptyCase
           , ScopedTypeVariables
           , UndecidableInstances
           , TypeFamilies -- for equality constraints
  #-}


module Data.Functor.Adjunction where

import Data.Functor.Identity (Identity (..))
import Data.Proxy
import Data.Coerce (coerce)

#if __GLASGOW_HASKELL__ >= 702
import qualified GHC.Generics as G
#endif

import Data.Functor.Rep


class (Functor m, Representable r)=> Adjunction m r | m -> r, r -> m where
    {-^ This class defines "canonical" adjunctions. Because of the functional dependences, two isomorphic types cannot be adjoint to the same functor.
    -}
    leftAdjunct :: (m a -> b) -> a -> r b
    rightAdjunct :: (a -> r b) -> m a -> b

    default leftAdjunct ::
        (gm ~ G.Rep1 m, gr ~ G.Rep1 r, G.Generic1 m, G.Generic1 r, Adjunction gm gr)=>
        (m a -> b) -> a -> r b
    leftAdjunct = gleftAdjunct

    default rightAdjunct ::
        (gm ~ G.Rep1 m, gr ~ G.Rep1 r, G.Generic1 m, G.Generic1 r, Adjunction gm gr)=>
        (a -> r b) -> m a -> b
    rightAdjunct = grightAdjunct

unit :: (Adjunction m r)=> a -> r (m a)
unit =  leftAdjunct id

counit :: (Adjunction m r)=> m (r a) -> a
counit = rightAdjunct id


instance Adjunction Identity Identity where
    leftAdjunct = coerce
    rightAdjunct = coerce

instance Adjunction ((,) c) ((->) c) where
    leftAdjunct f y x = f (x, y)
    rightAdjunct f (y, x) = f x y


#if __GLASGOW_HASKELL__ >= 702

--- Generics ---
instance Adjunction G.V1 G.U1 where
    leftAdjunct _ _ = G.U1
    rightAdjunct _ void = case void of {}

instance Adjunction G.Par1 G.Par1 where
    leftAdjunct = coerce
    rightAdjunct = coerce

-- This needs UndecidableInstances:
instance (Adjunction m r)=> Adjunction (G.Rec1 m) (G.Rec1 r) where
    leftAdjunct = l
      where
        l :: forall a b. (G.Rec1 m a -> b) -> a -> G.Rec1 r b
        l = coerce (leftAdjunct :: (m a -> b) -> a -> r b)
    rightAdjunct = r
      where
        r :: forall a b. (a -> G.Rec1 r b) -> G.Rec1 m a -> b
        r = coerce (rightAdjunct :: (a -> r b) -> m a -> b)

instance
    (Adjunction m r, Adjunction m' r')=> Adjunction ((G.:+:) m m') ((G.:*:) r r')
  where
    leftAdjunct f x = leftAdjunct (f . G.L1) x G.:*: leftAdjunct (f . G.R1) x
    rightAdjunct f s = case s of
        G.L1 mnx -> rightAdjunct ((\(mx G.:*: _) -> mx) . f) mnx
        G.R1 mnx -> rightAdjunct ((\(_ G.:*: nx) -> nx) . f) mnx

instance
    (Adjunction m r, Adjunction m' r')=> Adjunction ((G.:.:) m' m) ((G.:.:) r r')
  where
    leftAdjunct f = G.Comp1 . leftAdjunct (leftAdjunct (f . G.Comp1))
    {-# INLINE leftAdjunct #-}
    rightAdjunct f = rightAdjunct (rightAdjunct (G.unComp1 . f)) . G.unComp1
    {-# INLINE rightAdjunct #-}

-- Instance for M1 intentionally left out, which makes this useless.

gleftAdjunct ::
    (gm ~ G.Rep1 m, gr ~ G.Rep1 r, G.Generic1 m, G.Generic1 r, Adjunction gm gr)=>
    (m a -> b) -> a -> r b
gleftAdjunct f = G.to1 . leftAdjunct (f . G.to1)
{-# INLINE gleftAdjunct #-}

grightAdjunct ::
    (gm ~ G.Rep1 m, gr ~ G.Rep1 r, G.Generic1 m, G.Generic1 r, Adjunction gm gr)=>
    (a -> r b) -> m a -> b
grightAdjunct f = rightAdjunct (G.from1 . f) . G.from1
{-# INLINE grightAdjunct #-}

#endif
