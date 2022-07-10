

module Control.Lens.Combinators where


import Data.Profunctor.Internal

import Data.Functor.Contravariant
import Data.Coerce (coerce)


to :: (Profunctor p, Contravariant m)=> (s -> a) -> p a (m a) -> p s (m s)
to f = dimap f (contramap f)
{-# INLINE to #-}
