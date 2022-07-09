{-# LANGUAGE ScopedTypeVariables
  #-}


module Data.Bifunctor.Clown where


import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Profunctor.Internal
import Data.Functor
import Data.Functor.Contravariant
import Data.Coerce (coerce)

newtype Clown m a b = Clown (m a)
runClown :: Clown m a b -> m a
runClown (Clown ma) = ma


instance (Traversable m)=> Bitraversable (Clown m) where
    bitraverse f _ (Clown mx) = fmap Clown (traverse f mx)

instance (Foldable m)=> Bifoldable (Clown m) where
    bifoldMap f _ (Clown mx) = foldMap f mx

instance (Functor m)=> Bifunctor (Clown m) where
    first = coerce (fmap :: (a -> b) -> m a -> m b) ::
        forall a b c. (a -> b) -> Clown m a c -> Clown m b c
    second = fmap

instance (Contravariant m)=> Profunctor (Clown m) where
    lmap = coerce (contramap :: (a -> b) -> m b -> m a) :: forall a b c. (a -> b) -> Clown m b c -> Clown m a c
    rmap = fmap


instance Functor (Clown m a) where fmap _ = coerce

instance Contravariant (Clown m a) where contramap _ = coerce
