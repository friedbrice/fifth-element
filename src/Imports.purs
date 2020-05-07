module Imports
  ( module Imports
  , module Prelude
  , module Control.Alternative
  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.Maybe.Trans
  , module Control.Monad.Trans.Class
  , module Control.MonadPlus
  , module Data.Either
  , module Data.Enum
  , module Data.Foldable
  , module Data.FoldableWithIndex
  , module Data.Functor
  , module Data.FunctorWithIndex
  , module Data.Generic.Rep
  , module Data.Generic.Rep.Bounded
  , module Data.Generic.Rep.Enum
  , module Data.Generic.Rep.Eq
  , module Data.Generic.Rep.HeytingAlgebra
  , module Data.Generic.Rep.Monoid
  , module Data.Generic.Rep.Ord
  , module Data.Generic.Rep.Ring
  , module Data.Generic.Rep.Semigroup
  , module Data.Generic.Rep.Semiring
  , module Data.Generic.Rep.Show
  , module Data.Int
  , module Data.Lens
  , module Data.Lens.Iso.Newtype
  , module Data.Lens.Record
  , module Data.List
  , module Data.Map
  , module Data.Maybe
  , module Data.Monoid.Additive
  , module Data.Monoid.Conj
  , module Data.Monoid.Disj
  , module Data.Monoid.Dual
  , module Data.Monoid.Endo
  , module Data.Monoid.Multiplicative
  , module Data.Newtype
  , module Data.NonEmpty
  , module Data.Ord.Down
  , module Data.Ord.Max
  , module Data.Ord.Min
  , module Data.Semigroup.First
  , module Data.Semigroup.Foldable
  , module Data.Semigroup.Last
  , module Data.Set
  , module Data.String
  , module Data.Traversable
  , module Data.TraversableWithIndex
  , module Data.Tuple
  , module Data.Tuple.Nested
  , module Data.Unfoldable
  , module Effect
  , module Effect.Console
  ) where

import Prelude


----
-- Language
----

import Control.Monad.Trans.Class (lift)
import Data.Generic.Rep (class Generic)
import Data.Int (even, toNumber)
import Data.Newtype (class Newtype, ala, alaF, un, unwrap, wrap)
import Effect (Effect)
import Effect.Console (log)

import Data.Generic.Rep.Bounded
  ( class GenericBottom
  , class GenericTop
  , genericBottom
  , genericTop
  )

import Data.Generic.Rep.Enum
  ( class GenericBoundedEnum
  , class GenericEnum
  , genericCardinality
  , genericFromEnum
  , genericPred
  , genericSucc
  , genericToEnum
  )

import Data.Generic.Rep.Eq
  ( class GenericEq
  , genericEq
  )

import Data.Generic.Rep.HeytingAlgebra
  ( class GenericHeytingAlgebra
  , genericConj
  , genericDisj
  , genericFF
  , genericImplies
  , genericNot
  , genericTT
  )

import Data.Generic.Rep.Monoid
  ( class GenericMonoid
  , genericMempty
  )

import Data.Generic.Rep.Ord
  ( class GenericOrd
  , genericCompare
  )

import Data.Generic.Rep.Ring
  ( class GenericRing
  , genericSub
  )

import Data.Generic.Rep.Semigroup
  ( class GenericSemigroup
  , genericAppend
  )

import Data.Generic.Rep.Semiring
  ( class GenericSemiring
  , genericAdd
  , genericMul
  , genericOne
  , genericZero
  )

import Data.Generic.Rep.Show
  ( class GenericShow
  , class GenericShowArgs
  , genericShow
  , genericShowArgs
  )

import Data.Monoid
  ( guard -- guardM
  ) as Monoid


----
-- Named Instances
----

import Data.Semigroup.First (First(First))
import Data.Semigroup.Last (Last(Last))

import Data.Monoid.Additive (Additive(Additive))
import Data.Monoid.Conj (Conj(Conj))
import Data.Monoid.Disj (Disj(Disj))
import Data.Monoid.Dual (Dual(Dual))
import Data.Monoid.Endo (Endo(Endo))
import Data.Monoid.Multiplicative (Multiplicative(Multiplicative))

import Data.Ord.Down (Down(Down))
import Data.Ord.Max (Max(Max))
import Data.Ord.Min (Min(Min))

import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)


----
-- Data Structures
----

import Data.Array
  ( fromFoldable -- mkArray
  , toUnfoldable -- rmArray
  ) as Array

import Data.NonEmpty
  ( NonEmpty(NonEmpty)
  , fromNonEmpty
  , (:|)
  )

import Data.List
  ( List(Nil, Cons)
  , head
  , tail
  , last
  , init
  , reverse
  , filter
  , filterM
  , mapMaybe
  , catMaybes
  , sort
  , sortBy
  , drop
  , dropWhile
  , take
  , takeWhile
  , span
  , partition
  , group
  , nub
  , nubBy
  , delete
  , deleteBy
  , zipWith
  , zipWithA
  , zip
  , unzip
  , (:)
  , (!!)
  )

import Data.List
  ( fromFoldable -- mkList
  , toUnfoldable -- rmList
  ) as List

import Data.Map
  ( Map
  , keys
  , lookup
  , alter
  , unionWith
  , intersectionWith
  , submap
  )

import Data.Map
  ( fromFoldable -- mkMap
  , toUnfoldable -- rmMap
  ) as Map

import Data.Set
  ( Set
  , insert
  , member
  , difference
  , subset
  , properSubset
  , union
  , intersection
  )

import Data.Set
  ( map -- mapSet
  , delete -- remove
  , fromFoldable -- mkSet
  , toUnfoldable -- rmSet
  ) as Set

import Data.Maybe
  ( Maybe(Nothing, Just)
  , fromJust
  , fromMaybe
  , fromMaybe'
  , isJust
  , isNothing
  , maybe
  , maybe'
  , optional
  )

import Data.Either
  ( Either(Left, Right)
  , choose
  , either
  , fromLeft
  , fromRight
  , hush
  , isLeft
  , isRight
  , note
  , note'
  )

import Data.Tuple
  ( Tuple(Tuple)
  , fst
  , snd
  , curry
  , uncurry
  , swap
  )

import Data.Tuple.Nested (type (/\), (/\))

import Data.String
  ( Pattern(Pattern)
  , Replacement(Replacement)
  , stripPrefix
  , stripSuffix
  , splitAt
  , trim
  , toUpper
  , toLower
  , split
  , replace
  , replaceAll
  , joinWith
  )


----
-- Optics
----

import Data.Lens
  ( Iso, Iso', iso
  , Lens, Lens', lens, lens', set, (.~), view, (^.)
  , Prism, Prism', prism, prism', preview, (^?), review
  , Traversal, Traversal', over, (%~)
  )

import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)


----
-- Enumeration Classes
----

import Data.Enum
  ( class BoundedEnum
  , class Enum
  , Cardinality(..)
  , cardinality
  , defaultCardinality
  , defaultFromEnum
  , defaultPred
  , defaultSucc
  , defaultToEnum
  , downFrom
  , downFromIncluding
  , enumFromThenTo
  , enumFromTo
  , fromEnum
  , pred
  , succ
  , toEnum
  , toEnumWithDefaults
  , upFrom
  , upFromIncluding
  )


----
-- Functor Classes
----

import Data.Functor
  ( class Functor
  , flap
  , map
  , mapFlipped
  , void
  , voidLeft
  , voidRight
  , ($>)
  , (<#>)
  , (<$)
  , (<$>)
  , (<@>)
  )

import Control.Applicative
  ( class Applicative
  , class Apply
  , apply
  , liftA1
  , pure
  , unless
  , when
  , (*>)
  , (<*)
  , (<*>)
  )

import Control.Alternative
  ( class Alt
  , class Alternative
  , class Plus
  , alt
  , empty
  , (<|>)
  )

import Control.Monad
  ( class Bind
  , class Monad
  , ap
  , bind
  , ifM
  , join
  , liftM1
  , unlessM
  , whenM
  , (<=<)
  , (=<<)
  , (>=>)
  , (>>=)
  )

import Control.MonadPlus
  ( class MonadPlus
  , class MonadZero
  , guard
  )


----
-- Iteration Classes
----

import Data.Unfoldable
  ( class Unfoldable
  , class Unfoldable1
  , none
  , range
  , replicate
  , replicate1
  , replicate1A
  , replicateA
  , singleton
  , unfoldr
  , unfoldr1
  )

import Data.Unfoldable
  ( fromMaybe -- rmMaybe
  ) as Unfoldable

import Data.Foldable
  ( class Foldable
  , all
  , and
  , any
  , elem
  , find
  , findMap
  , fold
  , foldM
  , foldMap
  , foldl
  , foldr
  , for_
  , indexl
  , indexr
  , intercalate
  , length
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  , notElem
  , null
  , oneOf
  , oneOfMap
  , or
  , product
  , sequence_
  , sum
  , surround
  , surroundMap
  , traverse_
  )

import Data.Semigroup.Foldable
  ( class Foldable1
  , fold1
  , fold1Default
  , foldMap1
  , foldMap1Default
  , for1_
  , intercalateMap
  , sequence1_
  , traverse1_
  )

import Data.Semigroup.Foldable
  ( intercalate -- intercalate1
  , minimum -- minimum1
  , maximum -- maximum1
  ) as Foldable1

import Data.Traversable
  ( class Traversable
  , for
  , mapAccumL
  , mapAccumR
  , scanl
  , scanr
  , sequence
  , traverse
  )


----
-- Indexed Iteration Classes
----

import Data.FunctorWithIndex
  ( class FunctorWithIndex
  , mapWithIndex
  )

import Data.FoldableWithIndex
  ( class FoldableWithIndex
  , allWithIndex
  , anyWithIndex
  , findWithIndex
  , foldMapWithIndex
  , foldWithIndexM
  , foldlWithIndex
  , foldrWithIndex
  , forWithIndex_
  , surroundMapWithIndex
  , traverseWithIndex_
  )

import Data.TraversableWithIndex
  ( class TraversableWithIndex
  , forWithIndex
  , mapAccumLWithIndex
  , mapAccumRWithIndex
  , scanlWithIndex
  , scanrWithIndex
  , traverseWithIndex
  )

import Data.String.CodeUnits (singleton, toCharArray) as Internal
import Data.Enum (enumFromTo) as Internal

foreign import undefined :: forall a. a

foreign import debug :: forall a. String -> a -> a


----
-- Aliased Functions
----

mkArray :: forall f. Foldable f => f ~> Array
mkArray = Array.fromFoldable

rmArray :: forall f. Unfoldable f => Array ~> f
rmArray = Array.toUnfoldable

array :: Array ~> Array
array = identity

mkMap :: forall f k v. Ord k => Foldable f => f (Tuple k v) -> Map k v
mkMap = Map.fromFoldable

rmMap :: forall f k v. Unfoldable f => Map k v -> f (Tuple k v)
rmMap = Map.toUnfoldable

mkSet :: forall f a. Foldable f => Ord a => f a -> Set a
mkSet = Set.fromFoldable

rmSet :: forall f. Unfoldable f => Set ~> f
rmSet = Set.toUnfoldable

mapSet :: forall a b. Ord b => (a -> b) -> Set a -> Set b
mapSet = Set.map

remove :: forall a. Ord a => a -> Set a -> Set a
remove = Set.delete

mkList :: forall f. Foldable f => f ~> List
mkList = List.fromFoldable

rmList :: forall f. Unfoldable f => List ~> f
rmList = List.toUnfoldable

list :: List ~> List
list = identity

mkMaybe :: forall f. Foldable f => f ~> Maybe
mkMaybe = map unwrap <<< foldMap (Just <<< First)

rmMaybe :: forall f. Unfoldable f => Maybe ~> f
rmMaybe = Unfoldable.fromMaybe

mkString :: forall f. Foldable f => f Char -> String
mkString = foldMap Internal.singleton

rmString :: forall f. Unfoldable f => String -> f Char
rmString = rmArray <<< Internal.toCharArray

intercalate1 :: forall f m. Foldable1 f => Semigroup m => m -> f m -> m
intercalate1 = Foldable1.intercalate

maximum1 :: forall f a. Ord a => Foldable1 f => f a -> a
maximum1 = Foldable1.maximum

minimum1 :: forall f a. Ord a => Foldable1 f => f a -> a
minimum1 = Foldable1.minimum

flatMap :: forall f a b. Monad f => (a -> f b) -> f a -> f b
flatMap = (=<<)

guardM :: forall a. Monoid a => Boolean -> a -> a
guardM = Monoid.guard

infix 8 Internal.enumFromTo as ..

enum :: forall a. BoundedEnum a => List a
enum = bottom .. top
