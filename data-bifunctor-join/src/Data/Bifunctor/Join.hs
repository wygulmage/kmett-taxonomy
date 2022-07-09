


module Data.Bifunctor.Join where

import Data.Bitraversable
import Data.Bifoldable
import Data.Bifunctor


newtype Join p a = Join (p a a)
runJoin :: Join p a -> p a a
runJoin (Join pxy) = pxy

instance (Bitraversable p)=> Traversable (Join p) where
    traverse f (Join pxy) = fmap Join (bitraverse f f pxy)

instance (Bifoldable p)=> Foldable (Join p) where
    foldMap f (Join pxy) = bifoldMap f f pxy

instance (Bifunctor p)=> Functor (Join p) where
    fmap f (Join pxy) = Join (bimap f f pxy)
