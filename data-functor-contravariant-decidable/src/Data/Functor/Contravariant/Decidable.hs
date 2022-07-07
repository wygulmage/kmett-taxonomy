{-# LANGUAGE CPP #-}


module Data.Functor.Contravariant.Decidable where

import Data.Functor.Apply
import Data.Functor.Contravariant.Decide
import Data.Functor.Contravariant.Divisible
import Data.Functor.Contravariant.Conclude

import Data.Functor.Contravariant
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Void

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
import qualified GHC.Generics as G
#endif


class (Divisible m, Conclude m)=> Decidable m

lose :: (Decidable m)=> (a -> Void) -> m a
lose = conclude
{-# INLINE lose #-}

choose :: (Decidable m)=>  (a -> Either b c) -> m b -> m c -> m a
choose = decide
{-# INLINE choose #-}


instance (Monoid r)=> Decidable (Op r)

instance (Applicative m, Apply m, Decidable n)=> Decidable (Compose m n)
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 720
--- Generics ---
instance Decidable G.U1

instance (Decidable m)=> Decidable (G.Rec1 m)

instance (Decidable m)=> Decidable (G.M1 i info m)

instance (Monoid c)=> Decidable (G.K1 i c)

instance (Decidable m, Decidable n)=> Decidable ((G.:*:) m n)

-- This creates a dependency on Apply:
instance (Applicative m, Apply m, Decidable n)=> Decidable ((G.:.:) m n)

#endif
