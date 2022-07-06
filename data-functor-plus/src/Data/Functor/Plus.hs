{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
-- for Generics:
{-# LANGUAGE DefaultSignatures
           , GADTs
           , ScopedTypeVariables
           , Trustworthy -- lol
  #-}
#endif


module Data.Functor.Plus (
Plus (..),
) where


import Data.Functor.Alt

import Control.Monad (mzero)
import Data.Proxy (Proxy(..))

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
import qualified GHC.Generics as G
#endif


class (Alt m)=> Plus m where
    zero :: m a

    default zero ::
        (gr ~ G.Rep1 m, G.Generic1 m, Plus gr)=> m a
    zero = gzero

instance Plus IO where zero = mzero

instance Plus Proxy

instance Plus Maybe where zero = Nothing

instance Plus [] where zero = []

instance (Monoid c)=> Plus (Either c) where zero = Left mempty


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Plus G.U1 where zero = G.U1

instance (Plus m)=> Plus (G.Rec1 m) where zero = G.Rec1 zero

instance (Plus m)=> Plus (G.M1 i info m) where zero = G.M1 zero

gzero ::
    (gr ~ G.Rep1 m, G.Generic1 m, Plus gr)=> m a
gzero = G.to1 zero
{-# INLINE gzero #-}

#endif
