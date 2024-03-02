{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Test.Data.JoinSemilattice.Interval where

import           Data.JoinSemilattice.Interval (Interval (..), normalise)
import           Hedgehog                      (Gen, Property)
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import qualified Test.Util.Laws                as Laws

interval_int :: Gen (Interval Int)
interval_int = do
  let n = Gen.integral Range.linearBounded
  content <- fmap normalise ((,) <$> n <*> n)
  Gen.element [ Empty, Interval content, All ]

hprop_semigroup_associativity :: Property
hprop_semigroup_associativity = Laws.semigroup_associativity interval_int

hprop_monoid_identity :: Property
hprop_monoid_identity = Laws.monoid_identity interval_int

hprop_join_semilattice_commutativity :: Property
hprop_join_semilattice_commutativity = Laws.semigroup_commutativity interval_int

hprop_join_semilattice_idempotence :: Property
hprop_join_semilattice_idempotence = Laws.semigroup_idempotence interval_int

hprop_functor_identity :: Property
hprop_functor_identity = Laws.functor_identity interval_int

hprop_functor_composition :: Property
hprop_functor_composition = Laws.functor_composition interval_int

hprop_applicative_identity :: Property
hprop_applicative_identity = Laws.applicative_identity interval_int

hprop_applicative_composition :: Property
hprop_applicative_composition = Laws.applicative_composition interval_int

hprop_applicative_homomorphism :: Property
hprop_applicative_homomorphism = Laws.applicative_homomorphism @Interval

hprop_applicative_interchange :: Property
hprop_applicative_interchange = Laws.applicative_interchange interval_int

-- hprop_absR :: Property
-- hprop_absR = AbsR.absR_absR interval_int
