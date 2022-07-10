

module Data.Profunctor.Choice where


import Data.Profunctor.Internal
import Data.Profunctor.Types.Star
import Data.Tagged


class (Profunctor p)=> Choice p where
    right' :: p a b -> p (Either c a) (Either c b)


instance Choice (->) where right' = fmap

instance Choice Tagged where right' (Tagged x) = Tagged (Right x)
