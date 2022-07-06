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

module Data.Functor.Bind (
Bind (..), (-<<), (->-), (-<-), join, apDefault, liftB2,
) where

import Data.Functor.Apply
import Data.Tagged

import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
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


infixl 1 >>-
class (Apply m)=> Bind m where
    (>>-) :: m a -> (a -> m b) -> m b

    default (>>-) ::
        (gr ~ G.Rep1 m, G.Generic1 m, Bind gr)=>
        m a -> (a -> m b) -> m b
    (>>-) = gbind

infixr 1 -<<
(-<<) :: (Bind m)=> (a -> m b) -> m a -> m b
(-<<) = flip (>>-)
{-# INLINE (-<<) #-}

join :: (Bind m)=> m (m a) -> m a
join = (>>- id)

infixr 1 ->-
(->-) :: (Bind m)=> (a -> m b) -> (b -> m c) -> a -> m c
f ->- g = \ x -> f x >>- g
{-# INLINE (->-) #-}

infixr 1 -<-
(-<-) :: (Bind m)=> (b -> m c) -> (a -> m b) -> a -> m c
(-<-) = flip (->-)
{-# INLINE (-<-) #-}

apDefault :: (Bind m)=> m (a -> b) -> m a -> m b
apDefault = liftB2 id
{-# INLINE apDefault #-}

liftB2 :: (Bind m)=> (a -> b -> c) -> m a -> m b -> m c
{-^ @liftB2@ is a suitable definition of 'liftF2' when you have manually or generically defined '>>-' and 'fmap'.
-}
liftB2 f mx my = mx >>- \ x -> fmap (f x) my
{-# INLINE liftB2 #-}


instance Bind IO where (>>-) = (>>=)

instance Bind Identity

instance Bind Proxy

instance Bind Maybe where (>>-) = (>>=)

instance Bind [] where (>>-) = (>>=)

instance Bind NonEmpty where (>>-) = (>>=)

instance Bind (Either c) where
    mx >>- f = either Left f mx

instance (Semigroup c)=> Bind ((,) c) where
    (u, x) >>- f = case f x of
        (v, y) -> (u <> v, y)
    {-# INLINABLE (>>-) #-}

instance Bind (Tagged t)

instance Bind Seq.Seq where (>>-) = (>>=)


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Bind G.V1 where v >>- _ = case v of {}

instance Bind G.U1 where _ >>- _ = G.U1

instance Bind G.Par1 where G.Par1 x >>- f = f x

instance (Bind m)=> Bind (G.Rec1 m) where
    G.Rec1 mx >>- f = G.Rec1 (mx >>- (coerce f))
    {-# INLINE (>>-) #-}

instance (Bind m)=> Bind (G.M1 i info m) where
    G.M1 mx >>- f = G.M1 (mx >>- (coerce f))
    {-# INLINE (>>-) #-}

instance (Bind m, Bind n)=> Bind ((G.:*:) m n) where
    mx G.:*: nx >>- f =
        (G.:*:)
            (mx >>- ((\(my G.:*: _) -> my) . f))
            (nx >>- ((\(_ G.:*: ny) -> ny) . f))
    {-# INLINABLE (>>-) #-}

gbind ::
    (gr ~ G.Rep1 m, G.Generic1 m, Bind gr)=>
    m a -> (a -> m b) -> m b
gbind mx f = G.to1 (G.from1 mx >>- G.from1 . f)
{-# INLINE gbind #-}

#endif
