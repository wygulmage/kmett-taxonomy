{-# LANGUAGE RankNTypes -- GHC 6.8.1
  #-}
-- Because this requires RankNTypes, it shouldn't be a dependency of modules that don't require RankNTypes. Otherwise it would be a dependency of the whole `data-functor` hierarchy.


module Data.Functor.Yoneda (
Yoneda (..), runYoneda,
lowerYoneda, liftYoneda,
apYoneda,
) where

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Zip

import Control.Monad.Trans.Class
import Control.Monad.Reader.Class


newtype Yoneda m a = Yoneda (forall r. (a -> r) -> m r)
runYoneda :: Yoneda m a -> (a -> r) -> m r
runYoneda (Yoneda k) = k
{-# INLINE runYoneda #-}

lowerYoneda :: Yoneda m a -> m a
lowerYoneda = (`runYoneda` id)
{-# INLINABLE lowerYoneda #-}

liftYoneda :: (Functor m)=> m a -> Yoneda m a
liftYoneda mx = Yoneda (`fmap` mx)
{-# INLINABLE liftYoneda #-}


instance MonadTrans Yoneda where
    lift = liftYoneda
    {-# INLINE lift #-}

-- instance (MonadReader i m)=> MonadReader i (Yoneda m) where
--     ask = Yoneda reader
--     reader f = Yoneda (\ g -> reader (g . f))
--     local f (Yoneda k) = Yoneda (f . k)

instance (MonadPlus m)=> MonadPlus (Yoneda m)

instance (MonadZip m)=> MonadZip (Yoneda m) where
    -- mzipWith f mx my = zipWithYoneda f (lowerYoneda mx) (lowerYoneda my)
    mzipWith f mx = zipWithYoneda f mx . lowerYoneda
    {-# INLINABLE mzipWith #-}

-- zipWithYoneda :: (MonadZip m)=> (a -> b -> c) -> m a -> m b -> Yoneda m c
-- zipWithYoneda f mx my = Yoneda $ \ g -> mzipWith (\ x y -> g (f x y)) mx my

zipWithYoneda :: (MonadZip m)=> (a -> b -> c) -> Yoneda m a -> m b -> Yoneda m c
zipWithYoneda f mx my = Yoneda $ \ g ->
    mzipWith id (runYoneda mx (\ x y -> g $ f x y)) my
{-# INLINABLE zipWithYoneda #-}

instance (Fail.MonadFail m)=> Fail.MonadFail (Yoneda m) where
    fail str = Yoneda $ \_-> Fail.fail str
    {-# INLINE fail #-}

instance (Monad m)=> Monad (Yoneda m) where
    {-^ If possible, do not use 'Yoneda'\'s 'Monad' instance; instead, use a free 'Monad' like @Codensity@. -}
    (>>=) = bindYoneda . lowerYoneda
    {-# INLINE (>>=) #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}

bindYoneda :: (Monad m)=> m a -> (a -> Yoneda m b) -> Yoneda m b
bindYoneda mx f = Yoneda $ \ g -> mx >>= \ x -> runYoneda (f x) g
{-# INLINABLE bindYoneda #-}

instance (Alternative m)=> Alternative (Yoneda m) where
    empty = Yoneda $ pure empty
    {-# INLINE empty #-}
    Yoneda k1 <|> Yoneda k2 = Yoneda $ liftA2 (<|>) k1 k2
    {-# INLINE (<|>) #-}

instance (Applicative m)=> Applicative (Yoneda m) where
    {-^ If possible, do not use 'Yoneda'\'s 'Applicative' instance; instead, use a free 'Applicative' (or 'Monad'). -}
    pure x = Yoneda $ \ g -> pure $ g x
    {-# INLINE pure #-}
    (<*>) mf = apYoneda mf . lowerYoneda
    {-# INLINE (<*>) #-}
    mx <* my = Yoneda $ \ g -> runYoneda mx g <* lowerYoneda my
    {-# INLINE (<*) #-}
    (*>) = thenYoneda . lowerYoneda
    {-# INLINE (*>) #-}

apYoneda :: (Applicative m)=> Yoneda m (a -> b) -> m a -> Yoneda m b
apYoneda mf mx = Yoneda $ \ g -> runYoneda mf (g .) <*> mx
{-# INLINABLE apYoneda #-}

thenYoneda :: (Applicative m)=> m a -> Yoneda m b -> Yoneda m b
thenYoneda mx my = Yoneda $ \ g -> mx *> runYoneda my g
{-# INLINABLE thenYoneda #-}


instance Functor (Yoneda m) where
    fmap f (Yoneda k) = Yoneda $ \ g -> k (g . f)
    {-# INLINE fmap #-}
    x <$ Yoneda k = Yoneda $ \ g -> k $ \_-> g x
    {-# INLINE (<$) #-}
