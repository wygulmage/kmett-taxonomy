{-# LANGUAGE RankNTypes
  #-}
-- Because this requires RankNTypes, it shouldn't be a dependency of modules that don't require RankNTypes. Otherwise it would be a dependency of the whole `data-functor` hierarchy.


module Data.Functor.Yoneda where

import Control.Applicative
import Control.Monad

import Control.Monad.Trans.Class


newtype Yoneda m a = Yoneda (forall r. (a -> r) -> m r)
runYoneda :: Yoneda m a -> (a -> r) -> m r
runYoneda (Yoneda k) = k

lowerYoneda :: Yoneda m a -> m a
lowerYoneda mx = runYoneda mx id

liftYoneda :: (Functor m)=> m a -> Yoneda m a
liftYoneda mx = Yoneda (`fmap` mx)
{-# INLINABLE liftYoneda #-}


instance MonadTrans Yoneda where
    lift = liftYoneda
    {-# INLINE lift #-}

instance (MonadPlus m)=> MonadPlus (Yoneda m)

-- instance (MonadReader m)=> MonadReader (Yoneda m) where
--     reader f = Yoneda (\ g -> reader (g . f))

instance (Monad m)=> Monad (Yoneda m) where
    mx >>= f = Yoneda (\ g -> lowerYoneda mx >>= \ x -> runYoneda (f x) g)

instance (Alternative m)=> Alternative (Yoneda m) where
    empty = Yoneda (pure empty)
    Yoneda k1 <|> Yoneda k2 = Yoneda (liftA2 (<|>) k1 k2)

instance (Applicative m)=> Applicative (Yoneda m) where
    pure x = Yoneda (\ f -> pure (f x))
    -- liftA2 f mx my =
    --     Yoneda (\ g -> liftA2 (\ x y -> g (f x y)) (lowerYoneda mx) (lowerYoneda my))
    Yoneda kf <*> mx = Yoneda (\ g -> kf (g .) <*> lowerYoneda mx)

instance Functor (Yoneda m) where
    fmap f (Yoneda k) = Yoneda (\ g -> k (g . f))
    x <$ Yoneda k = Yoneda (\ g -> k (const (g x)))
