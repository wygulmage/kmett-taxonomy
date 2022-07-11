{-# LANGUAGE TypeFamilies
           , PolyKinds
  #-}

{- |
This module provides the 'Key' type family of @Data.Key@ in Edward Kmett's @keys@ package.
-}

module Data.Key.Family where

import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Sequence (Seq)

import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.Functor.Product as Functor
import qualified Data.Functor.Sum as Functor
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Monoid as Monoid
import Data.Proxy
import Data.Void

import qualified GHC.Generics as G


type family Key (m :: k -> Type) -- Could try making this more polykinded.

type instance Key Proxy = Void
type instance Key Identity = ()
type instance Key Monoid.Product = ()
type instance Key Monoid.Sum = ()
type instance Key Maybe = ()
type instance Key [] = Int
type instance Key NonEmpty = Int
-- no instance given for Key (Either c). I'd say ()...
-- instance for Key ((,) c) is c. I'd say ()..
type instance Key ((->) i) = i
type instance Key (Compose m n) = (Key m, Key n)
type instance Key (Functor.Product m n) = Either (Key m) (Key n)
type instance Key (Functor.Sum m n) = Either (Key m) (Key n)

type instance Key Seq = Int
type instance Key IntMap = Int
type instance Key (Map i) = i -- could be (Int, i)

type instance Key G.V1 = Void
type instance Key G.U1 = Void
type instance Key (G.K1 i c) = Void
type instance Key G.Par1 = ()
type instance Key (G.Rec1 m) = Key m
type instance Key (G.M1 i meta m) = Key m
type instance Key ((G.:*:) m n) = Either (Key m) (Key n)
type instance Key ((G.:+:) m n) = Either (Key m) (Key n)
type instance Key ((G.:.:) m n) = (Key m, Key n)
