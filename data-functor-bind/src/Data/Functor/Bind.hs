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
Bind (..), join,
) where

import Data.Functor.Apply
import Data.Tagged

import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy

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

join :: (Bind m)=> m (m a) -> m a
join = (>>- id)

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


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Bind G.V1 where v >>- _ = case v of {}

instance Bind G.U1 where _ >>- _ = G.U1

instance Bind G.Par1 where G.Par1 x >>- f = f x

instance (Bind m)=> Bind (G.Rec1 m) where
    -- G.Rec1 mx >>- f = G.Rec1 (mx >>- (G.unRec1 . f))
    G.Rec1 mx >>- f = G.Rec1 (mx >>- (coerce f))
    {-# INLINE (>>-) #-}

instance (Bind m)=> Bind (G.M1 i info m) where
    -- G.M1 mx >>- f = G.M1 (mx >>- (G.unM1 . f))
    G.M1 mx >>- f = G.M1 (mx >>- (coerce f))
    {-# INLINE (>>-) #-}

instance (Bind m, Bind n)=> Bind ((G.:*:) m n) where
    mx G.:*: nx >>- f =
        (G.:*:)
            (mx >>- ((\(my G.:*: _) -> my) . f))
            (nx >>- ((\(_ G.:*: ny) -> ny) . f))

gbind ::
    (gr ~ G.Rep1 m, G.Generic1 m, Bind gr)=>
    m a -> (a -> m b) -> m b
gbind mx f = G.to1 (G.from1 mx >>- G.from1 . f)
{-# INLINE gbind #-}

#endif
