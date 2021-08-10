{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module CSDC.User
  ( -- * Type
    User(..)
  , UserId
  ,

    -- * Class
    HasUser(..)
  ,

    -- * Transformer
    UserT
  , runUserT
  ) where

import           CSDC.Auth                      ( UserToken )
import qualified CSDC.Auth.ORCID               as ORCID
import           CSDC.Auth.User                 ( User(..) )
import           CSDC.DAO.Class                 ( HasCRUD(..)
                                                , HasDAO(..)
                                                , HasMessage(..)
                                                , HasRelation(..)
                                                )
import           CSDC.DAO.Types                 ( Member(..)
                                                , Person(..)
                                                , Subpart(..)
                                                , Unit(..)
                                                )
import           CSDC.Data.Id                   ( Id )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import           Control.Monad.Trans            ( MonadTrans(..) )

--------------------------------------------------------------------------------
-- Class

type UserId = User (Id Person)

-- | A class for monads that have an user in scope.
class Monad m => HasUser m where
  -- | Get the current user in scope.
  getUser :: m UserId

--------------------------------------------------------------------------------
-- Transformer

-- | A transformer that adds the 'HasUser' capability to another monad.
newtype UserT m a = UserT (ReaderT UserToken m a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadTrans,
      HasDAO
    )

deriving newtype instance HasCRUD Person m => HasCRUD Person (UserT m)

deriving newtype instance HasCRUD Unit m => HasCRUD Unit (UserT m)

deriving newtype instance HasRelation Member m => HasRelation Member (UserT m)

deriving newtype instance HasRelation Subpart m => HasRelation Subpart (UserT m)

deriving newtype instance HasMessage Member m => HasMessage Member (UserT m)

deriving newtype instance HasMessage Subpart m => HasMessage Subpart (UserT m)

instance HasDAO m => HasUser (UserT m) where
  getUser = UserT $ ask >>= \case
    Admin      -> pure Admin
    User token -> selectPersonORCID (ORCID.token_orcid token) >>= \case
      Nothing ->
        let person = Person { person_name        = ORCID.token_name token
                            , person_orcid       = ORCID.token_orcid token
                            , person_description = ""
                            }
        in  User <$> insert @Person person
      Just uid -> pure $ User uid

-- | Execute an 'UserT' in its base monad.
runUserT :: UserToken -> UserT m a -> m a
runUserT user (UserT act) = runReaderT act user
