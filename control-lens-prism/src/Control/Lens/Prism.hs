

module Control.Lens.Prism where


import Data.Profunctor.Internal
import Data.Profunctor.Choice


prism ::
    (Choice p, Applicative m)=>
    (b -> t) -> (s -> Either t a) -> p a (m b) -> p s (m t)
prism make match = dimap match (either pure (fmap make)) . right'
{-# INLINE prism #-}

_Left :: (Choice p, Applicative m)=> p a (m b) -> p (Either a c) (m (Either b c))
_Left = prism Left (either Right (Left . Right))

_Right :: (Choice p, Applicative m)=> p a (m b) -> p (Either c a) (m (Either c b))
_Right = prism Right (either (Left . Left) Right)

_Just :: (Choice p, Applicative m)=> p a (m b) -> p (Maybe a) (m (Maybe b))
_Just = prism Just (maybe (Left Nothing) Right)

_Nothing :: (Choice p, Applicative m)=> p () (m ()) -> p (Maybe a) (m (Maybe a))
_Nothing = prism (\_-> Nothing) (maybe (Right ()) (Left . Just))
