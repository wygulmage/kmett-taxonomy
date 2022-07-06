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

module Data.Functor.Alt (
Alt (..)
) where


import Data.Tagged

import Control.Applicative (liftA2)
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Proxy
import System.IO.Error

import Control.Monad.Trans.Reader (ReaderT (..), runReaderT)

import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.Sequence (Seq)

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
import qualified GHC.Generics as G
#endif
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 781
import Data.Coerce
#else
import Unsafe.coerce
#endif


class (Functor m)=> Alt m where
    (<!>) :: m a -> m a -> m a

    default (<!>) ::
      (gr ~ G.Rep1 m, G.Generic1 m, Alt gr)=>
      m a -> m a -> m a
    (<!>) = galt

instance Alt IO where
    act1 <!> act2 =
        act1 `catchIOError` \ e1 ->
            act2 `catchIOError` \ e2 ->
                if isUserError e2 && ioeGetErrorString e2 == "mzero"
                  then ioError e1
                  else ioError e2
    -- This is horrible, but to make <!> a monoid with mzero its neutral value, that's what we have to do.
    -- Would it make sense to check e1 for mzero first? (If it would prevents a space leak I think yes, otherwise no.)

instance Alt Identity

instance Alt Proxy

instance Alt Maybe where
    mx <!> my | null mx = my | otherwise = mx

instance Alt [] where (<!>) = (<>)

instance Alt NonEmpty where (<!>) = (<>)

instance (Semigroup c)=> Alt (Either c) where
    mx@Right{} <!> _ = mx
    Left x <!> Left y = Left (x <> y)
    _ <!> my = my
    {-# INLINABLE (<!>) #-}

instance Alt (Tagged t)


--- Transformers ---
instance (Alt m)=> Alt (ReaderT i m) where
    ReaderT kx <!> ReaderT ky =
        ReaderT (liftA2 (<!>) kx ky)

--- Containers ---
instance (Ord i)=> Alt (Map i) where
    (<!>) = (<>)
    {-# INLINE (<!>) #-}
instance Alt IntMap where (<!>) = (<>)
instance Alt Seq where (<!>) = (<>)


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Alt G.V1 where v1 <!> v2 = v1 `seq` case v2 of {}

instance Alt G.U1 where _ <!> _ = G.U1

instance Alt G.Par1 where x <!> _ = x

instance (Alt m)=> Alt (G.Rec1 m) where
    (<!>) = altRec1
      where
        altRec1 :: forall a. G.Rec1 m a -> G.Rec1 m a -> G.Rec1 m a
        altRec1 = coerce ((<!>) :: m a -> m a -> m a)
        {-# INLINE altRec1 #-}
    {-# INLINE (<!>) #-}

instance (Alt m)=> Alt (G.M1 i info m) where
    (<!>) = altM1
      where
        altM1 :: forall a. G.M1 i info m a -> G.M1 i info m a -> G.M1 i info m a
        altM1 = coerce ((<!>) :: m a -> m a -> m a)
        {-# INLINE altM1 #-}
    {-# INLINE (<!>) #-}

-- No instance for K1, because in general it should use ring addition rather than multiplication.

instance (Alt m, Alt n)=> Alt ((G.:*:) m n) where
    (mx G.:*: nx) <!> (my G.:*: ny) =
        (mx <!> my) G.:*: (nx <!> ny)
    {-# INLINABLE (<!>) #-}

-- Can't choose which side to privilege, so no instance for :+:.

-- Should :.: only use Alt for the outer context, or use Apply for the outer context to lift Alt into the inner context?

galt ::
    (gr ~ G.Rep1 m, G.Generic1 m, Alt gr)=>
    m a -> m a -> m a
galt mx my = G.to1 (G.from1 mx <!> G.from1 my)
{-# INLINE galt #-}

#endif
