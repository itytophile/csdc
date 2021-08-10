{-# LANGUAGE UndecidableInstances #-}

module CSDC.Aeson
  ( JSON(..)
  ) where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , GFromJSON
                                                , GToJSON
                                                , Options(fieldLabelModifier)
                                                , ToJSON(toJSON)
                                                , Zero
                                                , defaultOptions
                                                , genericParseJSON
                                                , genericToJSON
                                                )
import           GHC.Generics                   ( Generic(..) )

-- | A newtype for using with DerivingVia. It removes the prefix of records:
--
--   data Thing = Thing { some_thing1 :: Int, some_thing2 :: Bool }
--     deriving (FromJSON, ToJSON) via JSON Thing
--
--   toJSON (Thing 1 True) = { "thing1": 1, "thing2": true }
newtype JSON a = JSON a
  deriving (Show, Eq)

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (JSON a) where
  parseJSON val = JSON <$> genericParseJSON options val

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (JSON a) where
  toJSON (JSON a) = genericToJSON options a

options :: Options
options = defaultOptions { fieldLabelModifier = tail . dropWhile (/= '_') }
