{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

module Control.Lens.Combinators.Base (
ASetter, (%~), (.~),
Getting, (^.), (^..), toNonEmptyOf,
) where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Foldable
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid
import Data.Coerce (coerce)
-- import qualified GHC.Exts as Exts


type Getting r s a = (a -> Const r a) -> s -> Const r s

type ASetter s t a b = (a -> Identity b) -> s -> Identity t


foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf = coerce
{-# INLINE foldMapOf #-}

(^.) :: s -> Getting a s a -> a
s ^. o = foldMapOf o id s
{-# INLINE (^.) #-}

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf o f z s = appEndo (foldMapOf o (coerce f) s) z
{-# INLINE foldrOf #-}

-- (^..) :: forall s a. s -> (forall r. (a -> Const (Endo r) a) -> s -> Const (Endo r) s) -> [a]
-- s ^.. o = Exts.build (\ c n -> foldrOf o c n s)
(^..) :: s -> Getting (Endo [a]) s a -> [a]
s ^.. o = foldrOf o (:) [] s
{-# INLINE (^..) #-}

toNonEmptyOf :: Getting (NonEmptyDList a) s a -> s -> NonEmpty a
toNonEmptyOf o s = getNonEmptyDList (foldMapOf o singletonNonEmptyDList s) []

foldlOf' o f z s = foldrOf o f' id s z
  where
    f' x k z' = k $! f z' x
    {-# INLINE f' #-}
{-# INLINE foldlOf' #-}
-- foldlOf' o f z s = foldl' f z (s ^.. o)

foldMapOf' o f = foldlOf' o (\ z x -> z <> f x) mempty
{-# INLINE foldMapOf' #-}


(%~) :: ASetter s t a b -> (a -> b) -> s -> t
(%~) = coerce
{-# INLINE (%~) #-}

(.~) :: ASetter s t a b -> b -> s -> t
o .~ x = o %~ \_-> x
{-# INLINE (.~) #-}


--- INTERNAL ---
-- NonEmptyDList does not provide a way to create empty DLists, enforcing the nonempty invariant while using an "unsafe" regular internal representation.
newtype NonEmptyDList a = NonEmptyDList ([a] -> [a])
getNonEmptyDList :: NonEmptyDList a -> [a] -> NonEmpty a
getNonEmptyDList (NonEmptyDList k) xs = case k xs of
    x : xs' -> x :| xs'
    [] -> errorWithoutStackTrace "getNonEmptyDList: empty DList"

singletonNonEmptyDList :: a -> NonEmptyDList a
singletonNonEmptyDList x = NonEmptyDList (x :)

instance Semigroup (NonEmptyDList a) where
    NonEmptyDList k1 <> NonEmptyDList k2 = NonEmptyDList (k1 . k2)
