{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}
{-|
Module      : Data.JoinSemilattice.Interval
Description : Solving problems by narrowing down intervals of candidates.
Copyright   : (c) Jann Müller 2024
License     : MIT

-}
module Data.JoinSemilattice.Interval(
  Interval(..),
  isEmpty,
  point,
  tuple,
  normalise
) where

import           Control.Applicative                (liftA2)
import           Data.Input.Config                  (Config (..), Input (..))
import           Data.JoinSemilattice.Class.Abs     (AbsR (..))
import           Data.JoinSemilattice.Class.Boolean (BooleanR (..))
import           Data.JoinSemilattice.Class.Merge   (Merge (..), Result (..))
import           Data.Kind                          (Type)
import Data.JoinSemilattice.Class.Sum (SumR(..))
import Data.JoinSemilattice.Class.Eq(EqR(..))
import           Data.Monoid                        (Ap (..))
import           GHC.Generics                       (Generic)

{-| Defines intervals of possible values
-}
data Interval (x :: Type)
  = Interval (x, x) -- ^ The value is somewhere in this range (open-open)
  | Empty -- ^ No values
  | All -- ^ All values
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving (Bounded, Num) via (Ap Interval x)

boolItvl :: Interval Bool
boolItvl = Interval (False, True)

{-| If the interval only contains a single point, return it.
-}
point :: Eq x => Interval x -> Maybe x
point = \case
  Interval (x, y) | x == y -> Just x
  _                        -> Nothing

{-| Is the interval empty?
-}
isEmpty :: Interval x -> Bool
isEmpty = \case
  Empty -> True
  _     -> False

{-| Does the interval have defined boundaries?
-}
tuple :: Ord x => Interval x -> Maybe (x, x)
tuple = \case
  Interval k -> Just (normalise k)
  _          -> Nothing

normalise :: Ord x => (x, x) -> (x, x)
normalise (x, y) = (min x y, max x y)

instance Applicative Interval where
  pure a = Interval (a, a)

  Empty <*> _                         = Empty
  _ <*> Empty                         = Empty

  All <*> _                           = All
  _ <*> All                           = All

  Interval (f, g) <*> Interval (x, y) = Interval (f x, g y)

instance Ord x => Semigroup (Interval x) where
  Empty <> _ = Empty
  _ <> Empty = Empty

  All <> x = x
  x <> All = x

  -- intersection
  -- this is where we need to normalise the interval before
  -- comparing the bounds
  Interval (normalise -> (x, y)) <> Interval (normalise -> (x', y'))
    | y < x' || x > y' = Empty
    | otherwise        = Interval (max x x', min y y')

instance Ord x => Monoid (Interval x) where
  mempty = All

instance Fractional x => Fractional (Interval x) where
  (/) = liftA2 (/)

  fromRational = pure . fromRational
  recip        = fmap recip

instance (Ord x, Num x, Integral x) => Input (Interval x) where
  type Raw (Interval x) = Maybe (x, x)

  from i = Config
    { initial = pure All -- fmap (maybe Empty Interval) i
    , refine = pure . \case
        All -> maybe Empty Interval <$> i
        Interval (a, b)
          | any (not . isEmpty . (<>) (Interval (a, b)) . maybe Empty Interval) i ->
              if a /= b
                then let midpoint = (a + b) `div` 2 in [Interval (a, midpoint), Interval (midpoint, b)]
                else [Interval (a, b)]
          | otherwise -> [Empty]
        Empty -> [Empty]
    }

instance Ord x => Merge (Interval x) where
  Empty <<- _ = Failure
  _ <<- Empty = Failure

  _ <<- All   = Unchanged
  All <<- x   = Changed x

  Interval (normalise -> (x, y)) <<- Interval (normalise -> (x', y'))
    | (x, y) == (x', y') = Unchanged
    | y < x' || x > y'   = Failure
    | otherwise          = Changed $ Interval (max x x', min y y')

instance (Num x, Ord x) => AbsR (Interval x)
instance (Num x, Ord x) => SumR (Interval x)

instance BooleanR Interval where
  falseR = pure False
  trueR  = pure True
  notR (l, r) = (fmap not r, fmap not l)

  andR (x, y, z) =
    ( if | z == trueR                -> trueR
         | z == falseR && y == trueR -> falseR
         | otherwise                 -> boolItvl
    , if | z == trueR                -> trueR
         | z == falseR && x == trueR -> falseR
         | otherwise                 -> boolItvl
    , liftA2 (&&) x y -- ??
    )

  orR (x, y, z) =
    ( if | z == falseR               -> falseR
         | z == trueR && y == falseR -> trueR
         | otherwise                 -> boolItvl
    , if | z == falseR               -> falseR
         | z == trueR && x == falseR -> trueR
         | otherwise                 -> boolItvl
    , liftA2 (||) x y -- ??
    )

instance EqR Interval where
  type EqC Interval = Ord

  eqR (x, y, z) =
    ( if | z == trueR  -> y
         | otherwise   -> mempty
    , if | z == trueR  -> y
         | otherwise   -> mempty
    , let r = x <> y in
      if | isEmpty r -> falseR
         | otherwise -> boolItvl
    )