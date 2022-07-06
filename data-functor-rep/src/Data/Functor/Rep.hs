{-# LANGUAGE CPP
           , TypeFamilies
           , ScopedTypeVariables
           , DefaultSignatures
           , UndecidableInstances
  #-}


module Data.Functor.Rep where


import Data.Distributive
import Data.Functor.Identity (Identity (..))
import Data.Proxy
import Data.Void
import Data.Coerce (coerce)

import Data.Tagged

#if __GLASGOW_HASKELL__ >= 702
import qualified GHC.Generics as G
#endif


class (Distributive m)=> Representable m where
    type Rep m
    type Rep m = Rep (G.Rep1 m) -- UndecidableInstances
    tabulate :: (Rep m -> a) -> m a
    index :: m a -> Rep m -> a

    default tabulate ::
       (g ~ G.Rep1 m, G.Generic1 m, Rep m ~ Rep g, Representable g)=>
       (Rep m -> a) -> m a
    tabulate = gtabulate
    default index ::
       (g ~ G.Rep1 m, G.Generic1 m, Rep m ~ Rep g, Representable g)=>
        m a -> Rep m -> a
    index = gindex


instance Representable ((->) c) where
    type Rep ((->) c) = c
    tabulate = id
    index = id

instance Representable Identity
    -- type Rep Identity = ()
    -- tabulate f = Identity (f ())
    -- index (Identity x) () = x

instance Representable Proxy
    -- type Rep Proxy = Void
    -- tabulate _ = Proxy
    -- index Proxy = absurd

instance Representable (Tagged t) where
    -- Generic version uses (), not Proxy. Not sure whether that's fixable.
    type Rep (Tagged t) = Proxy t
    tabulate = unproxy
    index = proxy


collectRep :: (Representable m, Functor n)=> (a -> m b) -> n a -> m (n b)
collectRep f nx = tabulate (\ y -> fmap (flip (index . f) y) nx)
{-# INLINE collectRep #-}

bindRep :: (Representable m)=> m a -> (a -> m b) -> m b
bindRep mx f = tabulate (index mx >>= index . f)
{-# INLINE bindRep #-}

pureRep :: (Representable m)=> a -> m a
pureRep x = tabulate (pure x)
{-# INLINE pureRep #-}

apRep :: (Representable m)=> m (a -> b) -> m a -> m b
apRep mf mx = tabulate (\ z -> (index mf z) (index mx z))
{-# INLINE apRep #-}

liftR2 :: (Representable m)=> (a -> b -> c) -> m a -> m b -> m c
liftR2 f mx my = tabulate (\ z -> f (index mx z) (index my z))
{-# INLINE liftR2 #-}

fmapRep :: (Representable m)=> (a -> b) -> m a -> m b
fmapRep f mx = tabulate (f . index mx)
{-# INLINE fmapRep #-}

extendRep :: (i ~ Rep m, Representable m, Semigroup i)=> (m a -> b) -> m a -> m b
extendRep f mx = tabulate (f . tabulate . \ y -> (index mx . (<>) y))
{-# INLINE extendRep #-}

extractRep :: (i ~ Rep m, Representable m, Monoid i)=> m a -> a
extractRep mx = index mx mempty
{-# INLINE extractRep #-}

localRep :: (Representable m)=> (Rep m -> Rep m) -> m a -> m a
localRep f mx = tabulate (index mx . f)
{-# INLINE localRep #-}


#if __GLASGOW_HASKELL__ >= 702
--- GHC Generics ---
instance Representable G.U1 where
    type Rep G.U1 = Void
    tabulate _ = G.U1
    index G.U1 = absurd

instance Representable G.Par1 where
    type Rep G.Par1 = ()
    tabulate f = G.Par1 (f ())
    index (G.Par1 x) () = x

instance (Representable m)=> Representable (G.Rec1 m) where
    type Rep (G.Rec1 m) = Rep m
    tabulate = tab
      where
        tab :: forall a. (Rep m -> a) -> G.Rec1 m a
        tab = coerce (tabulate :: (Rep m -> a) -> m a)
        {-# INLINE tab #-}
    {-# INLINABLE tabulate #-}
    index = ind
      where
        ind :: forall a. G.Rec1 m a -> Rep m -> a
        ind = coerce (index :: m a -> Rep m -> a)
        {-# INLINE ind #-}
    {-# INLINABLE index #-}

instance (Representable m)=> Representable (G.M1 i meta m) where
    type Rep (G.M1 i meta m) = Rep m
    tabulate = tab
      where
        tab :: forall a. (Rep m -> a) -> G.M1 i meta m a
        tab = coerce (tabulate :: (Rep m -> a) -> m a)
        {-# INLINE tab #-}
    {-# INLINABLE tabulate #-}
    index = ind
      where
        ind :: forall a. G.M1 i meta m a -> Rep m -> a
        ind = coerce (index :: m a -> Rep m -> a)
        {-# INLINE ind #-}
    {-# INLINABLE index #-}

instance (Representable m, Representable n)=> Representable ((G.:*:) m n) where
    type Rep ((G.:*:) m n) = Either (Rep m) (Rep n)
    tabulate f = tabulate (f . Left) G.:*: tabulate (f . Right)
    {-# INLINABLE tabulate #-}
    index (mx G.:*: my) = either (index mx) (index my)
    {-# INLINABLE index #-}

instance (Representable m, Representable n)=> Representable ((G.:.:) m n) where
    type Rep ((G.:.:) m n) = (Rep m, Rep n)
    tabulate = G.Comp1 . tabulate . fmap tabulate . curry
    {-# INLINE tabulate #-}
    index (G.Comp1 mnx) = uncurry (index . index mnx)
    {-# INLINE index #-}

gtabulate ::
   (g ~ G.Rep1 m, G.Generic1 m, Rep m ~ Rep g, Representable g)=>
   (Rep m -> a) -> m a
gtabulate = G.to1 . tabulate
{-# INLINE gtabulate #-}

gindex ::
   (g ~ G.Rep1 m, G.Generic1 m, Rep m ~ Rep g, Representable g)=>
   m a -> Rep m -> a
gindex = index . G.from1
{-# INLINE gindex #-}

#endif
