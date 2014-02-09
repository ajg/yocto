-- Copyright 2014 Alvaro J. Genial [http://alva.ro]; see LICENSE file for more.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Derive.All
import Data.Derive.Arbitrary
import Data.DeriveTH
import Data.Map
import Data.Ratio
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Instances
import Text.JSON.Yocto


instance Arbitrary (Ratio Integer) where
  arbitrary = do
    d <- arbitrary :: Gen Integer
    return $ fromInteger d
    -- return (1 % 2)
    {- d <- arbitrary :: Gen Double
    return $ (toRational d :: Rational) -}


-- derive makeFunctor ''Value

derive makeArbitrary ''Value

prop_identity v = decode (encode v) == v

main = $(quickCheckAll)
