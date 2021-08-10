{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CSDC.Data.Id
  ( Id (..),
    zero,
    next,
    WithId (..),
  )
where

import CSDC.Aeson (JSON (..))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Web.Internal.HttpApiData (FromHttpApiData)

-- | A unique identifier for some type.
newtype Id a = Id {getId :: Int}
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, FromHttpApiData)

zero :: Id a
zero = Id 0

next :: Id a -> Id a
next (Id a) = Id (a + 1)

data WithId a = WithId
  { withId_id :: Id a,
    withId_value :: a
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via JSON (WithId a)
