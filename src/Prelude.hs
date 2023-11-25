{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Prelude (
  module PlutusTx.Prelude,
  module Data.Foldable,
  module Data.Functor,
  module Data.Void,
  module Control.Applicative,
  module GHC.Err,
  Applicative (..),
  Enum (..),
  Eq (..),
  FilePath,
  FromJSON,
  Generic,
  Int,
  Integral (toInteger),
  IO,
  Monoid (..),
  Num (..),
  Ord (..),
  PApplicative,
  PEnum,
  PEq,
  PFoldable,
  PFunctor,
  POrd,
  PMonoid,
  PSemigroup,
  PTraversable,
  Semigroup (..),
  Show (..),
  String,
  Text,
  ToJSON,
  Traversable (..),
  Typeable,
  (#<>),
  (#<$),
  (#<$>),
  (#<*>),
  (#<*),
  (#*>),
  (#==),
  (#/=),
  (#<),
  (#<=),
  (#>),
  (#>=),
  (#+),
  (#-),
  (#*),
  (/),
  drop,
  fromIntegral,
  fromJust,
  guard,
  init,
  last,
  mconcatMap,
  pabs,
  pall,
  pand,
  pany,
  pasum,
  pcompare,
  pconcat,
  pconcatMap,
  pelem,
  perror,
  pfind,
  pfmapDefault,
  pfoldMapDefault,
  pfold,
  pfoldl,
  pfoldMap,
  pfoldr,
  pfor,
  pfor_,
  pfromEnum,
  pfromInteger,
  plength,
  pliftA2,
  pmapM,
  pmappend,
  pmax,
  pmconcat,
  pmempty,
  pmin,
  pnegate,
  pnotElem,
  pnull,
  por,
  ppred,
  pproduct,
  ppure,
  psequence,
  psequenceA,
  psequenceA_,
  psucc,
  psum,
  ptoEnum,
  ptoList,
  ptraverse,
  ptraverse_,
  punless,
  putStrLn,
  print,
  printf,
)
where

import Control.Applicative
import Control.Monad (guard)
import Data.Aeson (FromJSON, ToJSON)
import Data.Eq (Eq (..))
import Data.Foldable
import Data.Functor
import Data.Int (Int)
import Data.List (drop, init, last)
import Data.List.Extra (mconcatMap)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid (..))
import Data.Ord (Ord (..))
import Data.Semigroup (Semigroup (..))
import Data.String (String)
import Data.Text (Text)
import Data.Traversable (Traversable (..))
import Data.Typeable (Typeable)
import Data.Void
import GHC.Enum (Enum (..))
import GHC.Err
import GHC.Generics (Generic)
import GHC.Num (Num (..))
import GHC.Real (Integral (..), fromIntegral, (/))
import GHC.Show (Show (..))
import PlutusTx.Prelude hiding (
  Applicative (..),
  Enum (..),
  Eq (..),
  Foldable (..),
  Functor (..),
  Monoid (..),
  Ord (..),
  Semigroup (..),
  Traversable (..),
  abs,
  all,
  and,
  any,
  asum,
  concat,
  concatMap,
  elem,
  error,
  find,
  fold,
  foldMap,
  foldl,
  foldlM,
  foldr,
  foldrM,
  for,
  for_,
  fromInteger,
  length,
  liftA2,
  mapM,
  mapM_,
  mappend,
  mconcat,
  negate,
  notElem,
  null,
  or,
  product,
  sequence,
  sequenceA,
  sequenceA_,
  sequence_,
  sum,
  toList,
  traverse_,
  unless,
  (*),
  (*>),
  (+),
  (-),
  (/=),
  (<$),
  (<$>),
  (<*),
  (<*>),
 )
import PlutusTx.Prelude qualified as P
import PlutusTx.Ratio qualified as P.Ratio
import System.IO (FilePath, IO, print, putStrLn)
import Text.Printf (printf)

{-# INLINEABLE perror #-}
perror :: () -> a
perror = P.error

-- | Plutus Tx version of '(Data.Functor.<$>)'.
{-# INLINEABLE (#<$>) #-}
(#<$>) :: (PFunctor f) => (a -> b) -> f a -> f b
(#<$>) = (P.<$>)

-- | Plutus Tx version of '(Data.Functor.<$)'.
{-# INLINEABLE (#<$) #-}
(#<$) :: (PFunctor f) => a -> f b -> f a
(#<$) = (P.<$)

infixl 4 #<$>

-- | Plutus Tx version of Eq
type PEq = P.Eq

{-# INLINEABLE (#==) #-}
(#==) :: (PEq a) => a -> a -> Bool
(#==) = (P.==)

{-# INLINEABLE (#/=) #-}
(#/=) :: (PEq a) => a -> a -> Bool
(#/=) = (P./=)

infix 4 #==, #/=

-- | Plutus Tx version of Data.Ord.Ord
type POrd = P.Ord

{-# INLINEABLE pcompare #-}
pcompare :: (POrd a) => a -> a -> Ordering
pcompare = P.compare

infix 4 #<, #<=, #>, #>=

{-# INLINEABLE (#<) #-}
(#<) :: (POrd a) => a -> a -> Bool
(#<) = (P.<)

{-# INLINEABLE (#<=) #-}
(#<=) :: (POrd a) => a -> a -> Bool
(#<=) = (P.<=)

{-# INLINEABLE (#>) #-}
(#>) :: (POrd a) => a -> a -> Bool
(#>) = (P.>)

{-# INLINEABLE (#>=) #-}
(#>=) :: (POrd a) => a -> a -> Bool
(#>=) = (P.>=)

{-# INLINEABLE pmax #-}
pmax :: (POrd a) => a -> a -> a
pmax = P.max

{-# INLINEABLE pmin #-}
pmin :: (POrd a) => a -> a -> a
pmin = P.min

-- | Plutus Tx version of Enum
type PEnum = P.Enum

{-# INLINEABLE psucc #-}
psucc :: (PEnum a) => a -> a
psucc = P.succ

{-# INLINEABLE ppred #-}
ppred :: (PEnum a) => a -> a
ppred = P.pred

{-# INLINEABLE ptoEnum #-}
ptoEnum :: (PEnum a) => Integer -> a
ptoEnum = P.toEnum

{-# INLINEABLE pfromEnum #-}
pfromEnum :: (PEnum a) => a -> Integer
pfromEnum = P.fromEnum

{-# INLINEABLE (#+) #-}
(#+) :: (AdditiveMonoid a) => a -> a -> a
(#+) = (P.+)

{-# INLINEABLE (#-) #-}
(#-) :: (AdditiveGroup a) => a -> a -> a
(#-) = (P.-)

infixl 6 #+, #-

{-# INLINEABLE (#*) #-}
(#*) :: (MultiplicativeSemigroup a) => a -> a -> a
(#*) = (P.*)

infixl 7 #*

-- | Plutus Tx version of 'GHC.Num.abs'
{-# INLINEABLE pabs #-}
pabs :: (P.Ord n, AdditiveGroup n) => n -> n
pabs = P.abs

{-# INLINEABLE pfromInteger #-}
pfromInteger :: Integer -> P.Ratio.Rational
pfromInteger = P.Ratio.fromInteger

-- | Plutus Tx version of 'GHC.Num.negate'
{-# INLINEABLE pnegate #-}
pnegate :: (AdditiveGroup a) => a -> a
pnegate = P.negate

-- | Plutus Tx version of Foldable
type PFoldable = P.Foldable

-- | Plutus Tx version of 'Data.Foldable.foldr'.
{-# INLINEABLE pfoldr #-}
pfoldr :: (PFoldable t) => (a -> b -> b) -> b -> t a -> b
pfoldr = P.foldr

-- | Plutus Tx version of 'Data.Foldable.fold'.
{-# INLINEABLE pfold #-}
pfold :: (PFoldable t, PMonoid m) => t m -> m
pfold = P.fold

-- | Plutus Tx version of 'Data.Foldable.foldMap'.
pfoldMap :: (PFoldable t, PMonoid m) => (a -> m) -> t a -> m
pfoldMap = P.foldMap

-- | Plutus Tx version of 'Data.Foldable.foldl'.
{-# INLINEABLE pfoldl #-}
pfoldl :: (PFoldable t) => (b -> a -> b) -> b -> t a -> b
pfoldl = P.foldl

-- | Plutus Tx version of 'Data.Foldable.toList'.
ptoList :: (PFoldable t) => t a -> [a]
{-# INLINE ptoList #-}
ptoList = P.toList

-- | Plutus Tx version of 'Data.Foldable.null'.
{-# INLINEABLE pnull #-}
pnull :: (PFoldable t) => t a -> Bool
pnull = P.null

-- | Plutus Tx version of 'Data.Foldable.length'.
{-# INLINEABLE plength #-}
plength :: (PFoldable t) => t a -> Integer
plength = P.length

-- | Plutus Tx version of 'Data.Foldable.sum'.
{-# INLINEABLE psum #-}
psum :: (PFoldable t, AdditiveMonoid a) => t a -> a
psum = P.sum

-- | Plutus Tx version of 'Data.Foldable.product'.
{-# INLINEABLE pproduct #-}
pproduct :: (PFoldable t, MultiplicativeMonoid a) => t a -> a
pproduct = P.product

-- | Plutus Tx version of 'Data.Foldable.traverse_'.
ptraverse_ :: (PFoldable t, PApplicative f) => (a -> f b) -> t a -> f ()
ptraverse_ = P.traverse_

-- | Plutus Tx version of 'Data.Foldable.for_'.
{-# INLINEABLE pfor_ #-}
pfor_ :: (PFoldable t, PApplicative f) => t a -> (a -> f b) -> f ()
pfor_ = P.for_

-- | Plutus Tx version of 'Data.Foldable.sequenceA_'.
{-# INLINEABLE psequenceA_ #-}
psequenceA_ :: (PFoldable t, PApplicative f) => t (f a) -> f ()
psequenceA_ = P.sequenceA_

-- | Plutus Tx version of 'Data.Foldable.asum'.
pasum :: (PFoldable t, Alternative f) => t (f a) -> f a
pasum = P.asum

-- | Plutus Tx version of 'Data.Foldable.concat'.
{-# INLINE pconcat #-}
pconcat :: (PFoldable t) => t [a] -> [a]
pconcat = P.concat

-- | Plutus Tx version of 'Data.Foldable.concatMap'.
{-# INLINE pconcatMap #-}
pconcatMap :: (PFoldable t) => (a -> [b]) -> t a -> [b]
pconcatMap = P.concatMap

-- | Plutus Tx version of 'Data.Foldable.and'.
{-# INLINEABLE pand #-}
pand :: (PFoldable t) => t Bool -> Bool
pand = P.and

-- | Plutus Tx version of 'Data.Foldable.or'.
{-# INLINEABLE por #-}
por :: (PFoldable t) => t Bool -> Bool
por = P.or

-- | Plutus Tx version of 'Data.Foldable.any'.
{-# INLINEABLE pany #-}
pany :: (PFoldable t) => (a -> Bool) -> t a -> Bool
pany = P.any

-- | Plutus Tx version of 'Data.Foldable.all'.
{-# INLINEABLE pall #-}
pall :: (PFoldable t) => (a -> Bool) -> t a -> Bool
pall = P.all

-- | Plutus Tx version of 'Data.Foldable.find'.
{-# INLINEABLE pfind #-}
pfind :: (PFoldable t) => (a -> Bool) -> t a -> Maybe a
pfind = P.find

-- | Plutus Tx version of Semigroup
type PSemigroup = P.Semigroup

infixr 6 #<>

-- | Plutus Tx version of '(Data.Semigroup.<>)'.
{-# INLINEABLE (#<>) #-}
(#<>) :: (PSemigroup a) => a -> a -> a
(#<>) = (P.<>)

-- | Plutus Tx version of Data.Monoid.Monoid
type PMonoid = P.Monoid

-- | Plutus Tx version of 'Data.Monoid.mempty'.
{-# INLINEABLE pmempty #-}
pmempty :: (PMonoid a) => a
pmempty = P.mempty

-- | Plutus Tx version of 'Data.Monoid.mappend'.
{-# INLINEABLE pmappend #-}
pmappend :: (P.Monoid a) => a -> a -> a
pmappend = (P.<>)

-- | Plutus Tx version of 'Data.Monoid.mconcat'.
{-# INLINEABLE pmconcat #-}
pmconcat :: (P.Monoid a) => [a] -> a
pmconcat = P.mconcat

-- | Plutus Tx version of Functor
type PFunctor = P.Functor

-- | Plutus Tx version of Applicative
type PApplicative = P.Applicative

{-# INLINEABLE ppure #-}
ppure :: (PApplicative f) => a -> f a
ppure = P.pure

{-# INLINEABLE pliftA2 #-}
pliftA2 :: (PApplicative f) => (a -> b -> c) -> f a -> f b -> f c
pliftA2 = P.liftA2

infixl 4 #<*>, #<*, #*>

{-# INLINEABLE (#<*>) #-}
(#<*>) :: (PApplicative f) => f (a -> b) -> f a -> f b
(#<*>) = (P.<*>)

{-# INLINEABLE (#<*) #-}
(#<*) :: (PApplicative f) => f a -> f b -> f a
(#<*) = (P.<*)

{-# INLINEABLE (#*>) #-}
(#*>) :: (PApplicative f) => f a -> f b -> f b
(#*>) = (P.*>)

-- | Plutus Tx version of 'Control.Monad.unless'.
{-# INLINEABLE punless #-}
punless :: (PApplicative f) => Bool -> f () -> f ()
punless = P.unless

-- | Plutus Tx version of Traversable
type PTraversable = P.Traversable

-- | Plutus Tx version of 'Data.Traversable.traverse'.
{-# INLINEABLE ptraverse #-}
ptraverse :: (PTraversable t, PApplicative f) => (a -> f b) -> t a -> f (t b)
ptraverse = P.traverse

-- | Plutus Tx version of 'Data.Traversable.sequenceA'.
{-# INLINE psequenceA #-}
psequenceA :: (PTraversable t, PApplicative f) => t (f a) -> f (t a)
psequenceA = P.sequenceA

-- | Plutus Tx version of 'Data.Traversable.sequence'.
{-# INLINE psequence #-}
psequence :: (PTraversable t, PApplicative f) => t (f a) -> f (t a)
psequence = P.sequence

-- | Plutus Tx version of 'Data.Traversable.mapM'.
{-# INLINE pmapM #-}
pmapM :: (PTraversable t, PApplicative f) => (a -> f b) -> t a -> f (t b)
pmapM = P.mapM

-- | Plutus Tx version of 'Data.Traversable.for'.
pfor :: (PTraversable t, PApplicative f) => t a -> (a -> f b) -> f (t b)
{-# INLINE pfor #-}
pfor = P.for

-- | Plutus Tx version of 'Data.Traversable.fmapDefault'.
pfmapDefault ::
  forall t a b.
  (PTraversable t) =>
  (a -> b) ->
  t a ->
  t b
{-# INLINE pfmapDefault #-}
pfmapDefault = P.fmapDefault

-- | Plutus Tx version of 'Data.Traversable.foldMapDefault'.
pfoldMapDefault ::
  forall t m a.
  (PTraversable t, PMonoid m) =>
  (a -> m) ->
  t a ->
  m
{-# INLINE pfoldMapDefault #-}
pfoldMapDefault = P.foldMapDefault

-- List

-- | Plutus Tx version of 'Data.List.elem'.
{-# INLINEABLE pelem #-}
pelem :: (PEq a) => a -> [a] -> Bool
pelem = P.elem

-- | Plutus Tx version of 'Data.List.notElem'.
{-# INLINEABLE pnotElem #-}
pnotElem :: (PEq a) => a -> [a] -> Bool
pnotElem = P.notElem
