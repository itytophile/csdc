{-# LANGUAGE TemplateHaskell #-}

module CSDC.Auth.User
  ( User(..)
  ) where

import           Data.Aeson.TH                  ( defaultOptions
                                                , deriveJSON
                                                )

-- | The current user of the server API. It can be either an admin or an user.
data User user = Admin | User user
  deriving (Show, Eq)

deriveJSON defaultOptions ''User
