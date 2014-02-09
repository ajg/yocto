-- Copyright 2014 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Applicative
import Control.Monad
import Data.Map
import Data.Ratio
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Instances
import Text.JSON.Yocto

{-
-- NOTE: This doesn't work because it either (a) enters infinite recursion or
         (b) creates ridiculously large data sets that never terminate in a
         practical memory space or time scale. So we have to create the
         instance manually (see below.)

import Data.Derive.All
import Data.Derive.Arbitrary
import Data.DeriveTH

derive makeArbitrary ''Value
-}

instance Arbitrary Value where
  arbitrary = sized value

value 0 = return Null
value n | n > 0 = oneof [
    Boolean <$> arbitrary,
    Number  <$> arbitrary,
    String  <$> arbitrary,
    Array   <$> replicateM m subvalue,
    Object  <$> fromList <$> replicateM m pair
  ]
  where subvalue = value m
        pair = (,) <$> arbitrary <*> subvalue
        m = n `div` 2

-- Limit rationals to those with finite decimal expansions.
-- TODO: Figure out a more intelligent way to do this.
instance Arbitrary Rational where
  arbitrary = oneof [
    (% 1) <$> arbitrary,
    (% 2) <$> arbitrary,
    (% 5) <$> arbitrary]

prop_identity v = decode (encode v) == v

main = $(quickCheckAll)
