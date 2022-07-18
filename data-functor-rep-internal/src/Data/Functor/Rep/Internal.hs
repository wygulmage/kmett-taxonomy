{-# LANGUAGE DefaultSignatures
           , EmptyCase
           , GeneralizedNewtypeDeriving
           , StandaloneDeriving
           , RankNTypes
           , TypeFamilies
  #-}

module Data.Functor.Rep.Internal where

import Data.HKD.Classes.FTraversable

import Data.Functor.Identity
import Data.Proxy
import Data.Void

import Data.Coerce (Coercible, coerce)
import GHC.Generics


class (Monad m)=> Representable m where
    scatter :: (FFunctor t)=> (t Identity -> r) -> (forall x. n x -> m x) -> t n -> m r

    default scatter ::
        (Generic1 m, rm ~ Rep1 m, Representable rm, FFunctor t)=>
        (t Identity -> r) -> (forall x. n x -> m x) -> t n -> m r
    scatter g f = to1 . scatter g (from1 . f)
    {-# INLINE scatter #-}

fdistribute :: (Representable m, FFunctor t)=> t m -> m (t Identity)
fdistribute = scatter id id

distribute :: (Representable m, Functor n)=> n (m a) -> m (n a)
-- So we need a t such that t m -> m (t Identity) ~ n (m a) -> m (n a).
-- It will look something like T n m -> m (T n Identity), or T a n m -> m (T a n Identity).
distribute = scatter (fmap runIdentity .# runFCompose) id .# FCompose


newtype FCompose a m n = FCompose (m (n a))
runFCompose :: FCompose a m n -> m (n a)
runFCompose (FCompose mna) = mna
instance (Functor m)=> FFunctor (FCompose a m) where
    ffmap = ((FCompose .) . (. runFCompose)) #. fmap


-- Base Instances ---
instance Representable Proxy
instance Representable Identity

instance Representable ((->) i) where
    -- scatter :: (t Identity -> r) -> (forall x. m x -> i -> x) -> t m -> i -> r
    scatter g f tm i = g (ffmap (\ mx -> Identity (f mx i)) tm)


--- GHC.Generics instances ---

-- absurdV1 :: V1 p -> a
-- absurdV1 v1 = case v1 of {}

-- instance Representable V1 where
--     scatter g f tn = case g (ffmap (Identity #. absurdV1 . f) tn) of {}
--     {-# INLINE scatter #-}

class GTabulate m where
    gtabulate :: (GKey m -> a) -> m a

type family GKey m where
    GKey ((->) i) = i
    GKey V1 = ()
    GKey U1 = Void
    GKey Par1 = ()
    GKey (Rec1 m) = GKey m
    GKey (M1 p info m) = GKey m
    GKey ((:.:) m1 m2) = (GKey m1, GKey m2)
    GKey ((:*:) m1 m2) = Either (GKey m1) (GKey m2)
    GKey ((:+:) m1 m2) = Either (GKey m1) (GKey m2)

type family GKey1 m where
    GKey1 ((->) i) = K1 () i -- Huh? Should I just use Const???
    GKey1 V1 = U1
    GKey1 U1 = V1
    GKey1 Par1 = U1
    GKey1 (Rec1 m)= GKey1 m
    GKey1 (M1 p info m) = GKey1 m
    GKey1 ((:.:) m1 m2) = (:*:) (GKey1 m1) (GKey1 m2)
    GKey1 ((:*:) m1 m2) = (:+:) (GKey1 m1) (GKey1 m2)
    GKey1 ((:+:) m1 m2) = (:+:) (GKey1 m1) (GKey1 m2)

instance GTabulate ((->) i) where gtabulate = id

instance GTabulate U1 where gtabulate _ = U1
instance Representable U1 where scatter _ _ _ = U1

instance GTabulate Par1 where gtabulate f = coerce f ()
instance Representable Par1 where
    scatter g f = Par1 #. g . ffmap ((Identity . unPar1) #. f)

deriving instance (GTabulate m)=> GTabulate (Rec1 m)
deriving instance (Representable m)=> Representable (Rec1 m)
deriving instance (GTabulate m)=> GTabulate (M1 i info m)
deriving instance (Representable m)=> Representable (M1 i info m)

fstPro1 :: (:*:) m1 m2 a -> m1 a
fstPro1 (m1 :*: _) = m1
sndPro1 :: (:*:) m1 m2 a -> m2 a
sndPro1 (_ :*: m2) = m2

instance (GTabulate m1, GTabulate m2)=> GTabulate ((:*:) m1 m2) where
    gtabulate f = gtabulate (f . Left) :*: gtabulate (f . Right)
instance (Representable m1, Representable m2)=> Representable ((:*:) m1 m2) where
    -- scatter :: (FFunctor t)=> (t Identity -> r) -> (forall x. n x -> (m1 :*: m2) x) -> t n -> (m1 :*: m2) r
    scatter g nt tn = let tp = ffmap nt tn in scatter g fstPro1 tp :*: scatter g sndPro1 tp

eitherSum1 :: (m1 a -> r) -> (m2 a -> r) -> (:+:) m1 m2 a -> r
eitherSum1 f _ (L1 m1) = f m1
eitherSum1 _ g (R1 m2) = g m2

-- instance (Representable m1, Representable m2)=> Representable ((:+:) m1 m2) where
--     -- scatter :: (FFunctor t)=> (t Identity -> r) -> (forall x. n x -> (m1 :+: m2) x) -> t n -> (m1 :+: m2) r
--     -- ffmap f :: t n -> t (m1 :*: m2)
--     -- scatter f id :: t m1 -> m1 r
--     -- scatter f id :: t m2 -> m2 r
--     -- scatter id :: (n x -> m1 x) -> t n -> m1 (t Identity)
--     -- scatter id :: (n x -> m2 x) -> t n -> m2 (t Identity)
--     scatter g f tn =


newtype F1 m = F1 (forall x. m x)

instance FFunctor F1 where ffmap f (F1 mx) = F1 (f mx)

--- Helpers ---
infixr 9 .#
( .# ) :: (Coercible b a)=> (b -> c) -> p a b -> a -> c
f .# _ = coerce f
{-# INLINE ( .# ) #-}

infixl 8 #.
( #. ) :: (Coercible c b)=> p b c -> (a -> b) -> a -> c
( #. ) _ = coerce
{-# INLINE ( #. ) #-}
