{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Test.Data.Input.Config where

import           Control.Monad.Holmes   (Holmes, runOne)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Hashable          (Hashable)
import           Data.Input.Config      (Config (..), Input (..), permute)
import qualified Data.Set               as Set
import           Data.Vector.Sized      (Vector)
import           GHC.TypeNats           (SNat, withSomeSNat)
import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

possibilities :: Gen [Int]
possibilities = do
  let values = Gen.int (Range.linear 0 100)
  set <- Gen.set (Range.linear 1 5) values

  pure (Set.toList set)

from_fill :: forall x. (Eq x, Hashable x, Input x, Show x, Raw x ~ Int) => Property
from_fill = property do
  count  <- forAll (fromIntegral <$> Gen.int (Range.linear 1 10))
  inputs <- forAll possibilities

  withSomeSNat count $ \snat -> mkProp @x snat inputs


mkProp :: forall x n. Raw x ~ Int => SNat n -> [Int] -> PropertyT IO ()
mkProp _ inputs = do
  let config :: Config (Vector n) Holmes x
      config = from inputs
  annotateShow (initial config)

  Just permutations <- liftIO (runOne (permute config))
  annotateShow (toList permutations)

  length (toList permutations) === length inputs ^ _
