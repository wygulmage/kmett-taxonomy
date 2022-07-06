{-# LANGUAGE GeneralizedNewtypeDeriving
           , DeriveDataTypeable
           , DeriveGeneric
           , CPP
  #-}


module Data.Tagged where


import Control.Applicative
import Control.Monad.Zip
import Data.Bits (Bits, FiniteBits)
import Data.Coerce (coerce)
import Data.Data (Data)
#if MIN_VERSION_base(4,10,0)
import Data.Bitraversable
import Data.Bifoldable
#endif
import Data.Bifunctor
import Data.Foldable
import Data.Functor.Classes
import Data.Ix (Ix)
import Data.Proxy
import qualified Data.String as String

import Foreign.Storable (Storable)

import GHC.Generics


newtype Tagged t a = Tagged a
  deriving
    ( Bounded, Enum, Eq, Ord, Ix
    , Data, Generic, Generic1
    , Floating, Fractional, Integral, Num, Real, RealFloat, RealFrac
    , String.IsString, Read, Show, Storable
    , Bits, FiniteBits
    , Semigroup, Monoid
    )
unTagged :: Tagged t a -> a
unTagged (Tagged x) = x


instance Monad (Tagged t) where Tagged x >>= f = f x

instance MonadZip (Tagged t) where
    mzipWith = coerce
    munzip = coerce

instance Applicative (Tagged t) where
    pure = Tagged
    liftA2 = coerce
    (<*>) = coerce

instance Bifunctor Tagged where
    bimap _ = coerce
    first _ = coerce
    second = fmap

instance Functor (Tagged t) where fmap = coerce

#if MIN_VERSION_base(4,10,0)
instance Bitraversable Tagged where
    bitraverse _ f (Tagged x) = fmap Tagged (f x)
    {-# INLINE bitraverse #-}

instance Bifoldable Tagged where
    bifoldMap _ = foldMap
    bifoldl _ = foldl
#endif

instance Traversable (Tagged t) where
    traverse f (Tagged x) = fmap Tagged (f x)
    {-# INLINE traverse #-}

instance Foldable (Tagged t) where
    foldMap = coerce
#if MIN_VERSION_base(4,13,0)
    foldMap' = coerce
#endif
    foldl = coerce
    foldl' = coerce

instance Eq1 (Tagged t) where liftEq = coerce
instance Eq2 Tagged where liftEq2 _ = coerce
instance Ord1 (Tagged t) where liftCompare = coerce
instance Ord2 Tagged where liftCompare2 _ = coerce


unproxy :: (Proxy t -> a) -> Tagged t a
{-^ Convert from a (lazy) 'Proxy'-based representation to a @Tagged@ representation.
-}
unproxy f = Tagged (f Proxy)

proxy :: Tagged t a -> proxy t -> a
{-^ Convert from a (strict) @Tagged@ representation to a (lazy) proxy-based representation.
-}
proxy (Tagged x) _ = x
