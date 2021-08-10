{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module CSDC.Error
  ( Error (..),
    IsError (..),
    HasError,
  )
where

import Control.Monad.Except (MonadError)

newtype Error
  = DAOError String
  deriving (Show, Eq)

class IsError e where
  toError :: e -> Error

type HasError e m = (MonadError Error m, IsError e)
