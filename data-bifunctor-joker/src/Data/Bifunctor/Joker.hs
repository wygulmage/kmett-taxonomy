{-# LANGUAGE ScopedTypeVariables
  #-}


module Data.Bifunctor.Joker where


import Data.Bitraversable
import Data.Bifoldable
import Data.Bifunctor
import Data.Profunctor.Internal
import Data.Coerce (coerce)


newtype Joker m a b = Joker (m b)
runJoker :: Joker m a b -> m b
runJoker (Joker mx) = mx


instance (Foldable m)=> Bifoldable (Joker m) where
    bifoldMap _ = foldMap

instance (Foldable m)=> Foldable (Joker m c) where
    foldMap = coerce (foldMap :: (a -> b) -> m a -> b)
        :: forall a b. (Monoid b)=> (a -> b) -> Joker m c a -> b

instance (Traversable m)=> Bitraversable (Joker m) where
    bitraverse _ f (Joker mx) = fmap Joker (traverse f mx)

instance (Traversable m)=> Traversable (Joker m c) where
    traverse f (Joker mx) = fmap Joker (traverse f mx)

instance (Functor m)=> Bifunctor (Joker m) where
    first _ = coerce
    second = fmap

instance (Functor m)=> Functor (Joker m c) where
    fmap = coerce (fmap :: (a -> b) -> m a -> m b)
        :: forall a b. (a -> b) -> Joker m c a -> Joker m c b
