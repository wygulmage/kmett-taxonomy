

module Control.Lens.Setter.Base where


import Data.Functor.Identity
import Data.Coerce (coerce)


type ASetter s t a b = (a -> Identity b) -> s -> Identity t

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
(%~) = coerce
{-# INLINE (%~) #-}

(.~) :: ASetter s t a b -> b -> s -> t
o .~ x = o %~ \_-> x
{-# INLINE (.~) #-}
