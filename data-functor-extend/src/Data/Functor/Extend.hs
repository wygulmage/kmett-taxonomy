{-# LANGUAGE CPP
  #-}
-- for Generics:
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
{-# LANGUAGE EmptyCase
           , ScopedTypeVariables
           , GADTs
           , DefaultSignatures
           , Trustworthy -- lol
  #-}
#endif


module Data.Functor.Extend (
Extend (..), duplicated,
) where


import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Proxy

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
import qualified GHC.Generics as G
#endif
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 781
import Data.Coerce
#else
import Unsafe.coerce
coerce = unsafeCoerce
#endif


class (Functor m)=> Extend m where
    extended :: (m a -> b) -> m a -> m b

    default extended :: (g ~ G.Rep1 m, G.Generic1 m, Extend g)=> (m a -> b) -> m a -> m b
    extended = gextended

duplicated :: (Extend m)=> m a -> m (m a)
duplicated = extended id


instance Extend Identity where
    extended = coerce

instance Extend Maybe where
    -- extended f mx | null mx = Nothing | otherwise = Just (f mx)

instance Extend Proxy

instance Extend NonEmpty where
    extended f xs@(x :| xs') = f xs :| loop xs'
      where
        loop (y : ys) = f (y :| ys) : loop ys
        loop [] = []

instance Extend [] where
    extended f = loop
      where
         loop xs@(x : xs') = f xs : loop xs'
         loop [] = []

instance (Semigroup c)=> Extend ((->) c) where
    -- f :: (c -> a) -> b
    -- g :: (c -> a)
    -- extended :: ((c -> a) -> b) -> (c -> a) -> c -> b
    extended f g x = f (g . (<>) x)
    {-# INLINABLE extended #-}

instance Extend ((,) c) where
    extended f ux = (fst ux, f ux)

instance Extend (Either c) where
    -- extended f cx = either Left (\_-> Right (f cx)) cx


#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Extend G.V1 where
    extended _ void = case void of {}

instance Extend G.U1 where
    extended _ _ = G.U1

instance Extend G.Par1 where
    extended = coerce

instance (Extend m)=> Extend (G.Rec1 m) where
    extended = extendedRec1
      where
        extendedRec1 :: forall a b. (G.Rec1 m a -> b) -> G.Rec1 m a -> G.Rec1 m b
        extendedRec1 = coerce (extended :: (m a -> b) -> m a -> m b)
        {-# INLINE extendedRec1 #-}
    {-# INLINE extended #-}

instance (Extend m)=> Extend (G.M1 i info m) where
    extended = extendedM1
      where
        extendedM1 :: forall a b. (G.M1 i info m a -> b) -> G.M1 i info m a -> G.M1 i info m b
        extendedM1 = coerce (extended :: (m a -> b) -> m a -> m b)
        {-# INLINE extendedM1 #-}
    {-# INLINE extended #-}

instance Extend (G.K1 i c) where
{-^ Law: extended f . extended g = extended (f . extended g)
This instance trivially satisfies the laws because it never applies the functions. Because 'ComonadApply' requires 'Comonad', we don't need to worry about creating incompatible instances.
-}
    extended _ (G.K1 u) = G.K1 u

instance (Extend m, Extend n)=> Extend ((G.:+:) m n) where
    extended f mnx = case mnx of
        G.L1 mx -> G.L1 (extended (f . G.L1) mx)
        G.R1 mx -> G.R1 (extended (f . G.R1) mx)
    {-# INLINABLE extended #-}


gextended :: (g ~ G.Rep1 m, G.Generic1 m, Extend g)=> (m a -> b) -> m a -> m b
gextended f = G.to1 . extended (f . G.to1) . G.from1

#endif
