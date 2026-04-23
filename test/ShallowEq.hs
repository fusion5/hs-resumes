{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module ShallowEq (ShallowEq, shallowEq, shouldBeShallow) where

import Control.Exception
import Control.Monad (unless)
import Data.CallStack
import Data.Functor.Compose
import Data.Text
import GHC.Generics
import Test.HUnit.Lang

{- | The purpose of ShallowEq is to provide a comparison similar to Eq but for some
constructors it should be able to omit some fields from the comparison. This is achieved
by defining custom ShallowEq instances.
-}
class ShallowEq a where
  shallowEq :: a -> a -> Bool
  -- Default signature uses GHC.Generics
  default shallowEq :: (Generic a, GShallowEq (Rep a)) => a -> a -> Bool
  shallowEq x y = gShallowEq (from x) (from y)

-- Helper class for generic representation
class GShallowEq f where
  gShallowEq :: f p -> f p -> Bool

-- Metadata (D1, C1, S1)
instance (GShallowEq f) => GShallowEq (M1 i c f) where
  gShallowEq (M1 x) (M1 y) = gShallowEq x y

-- Sum types (Choice between constructors)
instance (GShallowEq f, GShallowEq g) => GShallowEq (f :+: g) where
  gShallowEq (L1 x) (L1 y) = gShallowEq x y
  gShallowEq (R1 x) (R1 y) = gShallowEq x y
  gShallowEq _ _ = False

-- Product types (Multiple fields in a constructor)
instance (GShallowEq f, GShallowEq g) => GShallowEq (f :*: g) where
  gShallowEq (x1 :*: x2) (y1 :*: y2) = gShallowEq x1 y1 && gShallowEq x2 y2

-- Field values (K1): This is where the RECURSION happens.
-- We require that the field 'c' itself has a ShallowEq instance.
instance (ShallowEq c) => GShallowEq (K1 i c) where
  gShallowEq (K1 x) (K1 y) = shallowEq x y

-- Constructors with no fields (U1)
instance GShallowEq U1 where
  gShallowEq _ _ = True

instance (ShallowEq a, ShallowEq (b a)) => ShallowEq (Compose [] b a)
instance (ShallowEq a) => ShallowEq [a]
instance ShallowEq Text where shallowEq = (==)
instance ShallowEq Integer where shallowEq = (==)

shouldBeShallow :: (HasCallStack, ShallowEq a, Show a) => a -> a -> IO ()
shouldBeShallow actual expected = unless (shallowEq actual expected) $ do
  throwIO (HUnitFailure location $ ExpectedButGot Nothing expectedMsg actualMsg)
 where
  expectedMsg = Prelude.show expected
  actualMsg = Prelude.show actual

location :: (HasCallStack) => Maybe SrcLoc
location = case Prelude.reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing
