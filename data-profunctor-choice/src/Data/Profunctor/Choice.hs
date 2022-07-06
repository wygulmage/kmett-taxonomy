

module Data.Profunctor.Choice where


import Data.Profunctor.Internal
import Data.Tagged


class (Profunctor p)=> Choice p where
    second :: p a b -> p (Either c a) (Either c b)


instance Choice (->) where second = fmap

instance Choice Tagged where second (Tagged x) = Tagged (Right x)
