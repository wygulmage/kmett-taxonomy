{-# LANGUAGE DefaultSignatures
           , EmptyCase
           , KindSignatures
           , PolyKinds
           , RankNTypes
           , ScopedTypeVariables
           , TypeFamilies
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , StandaloneDeriving
  #-}

module Data.HKD.Classes.FApplicative where

import Data.Functor.Apply
import Data.HKD.Classes.FFunctor

import Data.Functor.Compose
import Data.Functor.Const
import qualified Data.Functor.Product as Functor
import qualified Data.Functor.Sum as Functor
import Data.Proxy

import Data.Coerce (Coercible, coerce)
import GHC.Generics


class (FFunctor t)=> FApply t where
    fliftA2 :: (forall x. m1 x -> m2 x -> m3 x) -> t m1 -> t m2 -> t m3

    default fliftA2 ::
        (Generic1 t, gr ~ Rep1 t, FApply gr)=>
        (forall x. m1 x -> m2 x -> m3 x) -> t m1 -> t m2 -> t m3
    fliftA2 f tm1 tm2 = to1 (fliftA2 f (from1 tm1) (from1 tm2))


class (FApply t)=> FApplicative t where
    fpure :: (forall x. m x) -> t m

    default fpure ::
        (Generic1 t, gr ~ Rep1 t, FApplicative gr)=>
        (forall x. m x) -> t m
    fpure m = to1 (fpure m)


instance FApply Proxy
instance FApplicative Proxy

instance (Semigroup c)=> FApply (Const c)
instance (Monoid c)=> FApplicative (Const c)

instance (FApply t1, FApply t2)=> FApply (Functor.Product t1 t2)
instance (FApplicative t1, FApplicative t2)=> FApplicative (Functor.Product t1 t2)


--- GHC.Generics ---

instance FApply V1 where fliftA2 _ v1 v2 = v1 `seq` case v2 of {}

instance FApply U1 where fliftA2 _ _ _ = U1
instance FApplicative U1 where fpure _ = U1

instance (Semigroup c)=> FApply (K1 p c) where
    fliftA2 _ = coerce ((<>) :: c -> c -> c)
    {-# INLINE fliftA2 #-}
instance (Monoid c)=> FApplicative (K1 p c) where
    fpure _ = K1 mempty
    {-# INLINE fpure #-}

deriving instance (FApply t)=> FApply (Rec1 t)
deriving instance (FApplicative t)=> FApplicative (Rec1 t)

deriving instance (FApply t)=> FApply (M1 p info t)
deriving instance (FApplicative t)=> FApplicative (M1 p info t)

instance (FApply t1, FApply t2)=> FApply ((:*:) t1 t2) where
    fliftA2 f (t1_1 :*: t2_1) (t1_2 :*: t2_2) =
        fliftA2 f t1_1 t1_2 :*: fliftA2 f t2_1 t2_2
    {-# INLINABLE fliftA2 #-}

instance (FApplicative t1, FApplicative t2)=> FApplicative ((:*:) t1 t2) where
    fpure m = fpure m :*: fpure m
    {-# INLINABLE fpure #-}

instance (Apply m, FApply t)=> FApply ((:.:) m t) where
    fliftA2 f = \ n1 n2 -> Comp1 (liftF2 (fliftA2 f) (unComp1 n1) (unComp1 n2))
    {-# INLINABLE fliftA2 #-}


--- Helpers ---

infixr 9 .#
( .# ) :: (Coercible b a)=> (b -> c) -> p a b -> a -> c
f .# _ = coerce f
{-# INLINE ( .# ) #-}

infixl 8 #.
( #. ) :: (Coercible c b)=> p b c -> (a -> b) -> a -> c
( #. ) _ = coerce
{-# INLINE ( #. ) #-}
