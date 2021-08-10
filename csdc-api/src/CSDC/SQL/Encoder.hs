{-# LANGUAGE OverloadedStrings #-}

module CSDC.SQL.Encoder
  ( -- * Base types
    text
    -- * Local types
  , id
  , idNullable
  , idList
  , orcidId
  , messageType
  , messageStatus
  , replyType
  , replyStatus
  ) where

import           CSDC.Prelude                   ( Id(getId)
                                                , MessageStatus(..)
                                                , MessageType(..)
                                                , ReplyStatus(..)
                                                , ReplyType(..)
                                                )
import           Prelude                 hiding ( id )

import qualified CSDC.Auth.ORCID               as ORCID

import           Data.Foldable                  ( foldl' )
import           Data.Functor.Contravariant     ( Contravariant(..) )
import           Data.Text                      ( Text )
import           Hasql.Encoders                 ( Params
                                                , array
                                                , dimension
                                                , element
                                                , nonNullable
                                                , nullable
                                                , param
                                                )

import qualified Hasql.Encoders                as Encoders

--------------------------------------------------------------------------------
-- Base types

text :: Params Text
text = param (nonNullable Encoders.text)

--------------------------------------------------------------------------------
-- Local types

id :: Params (Id a)
id = contramap (fromIntegral . getId) $ param (nonNullable Encoders.int4)

idNullable :: Params (Maybe (Id a))
idNullable =
  contramap (fmap (fromIntegral . getId)) $ param (nullable Encoders.int4)

idList :: Params [Id a]
idList = contramap (fmap (fromIntegral . getId))
  $ param (nonNullable (array (dim (element (nonNullable Encoders.int4)))))
  where dim = dimension foldl'

orcidId :: Params ORCID.Id
orcidId = contramap ORCID.getId $ param (nonNullable Encoders.text)

messageType :: Params MessageType
messageType = contramap encode text
 where
  encode Invitation = "Invitation"
  encode Submission = "Submission"

messageStatus :: Params MessageStatus
messageStatus = contramap encode text
 where
  encode Waiting  = "Waiting"
  encode Accepted = "Accepted"
  encode Rejected = "Rejected"

replyType :: Params ReplyType
replyType = contramap encode text
 where
  encode Accept = "Accept"
  encode Reject = "Reject"

replyStatus :: Params ReplyStatus
replyStatus = contramap encode text
 where
  encode Seen    = "Seen"
  encode NotSeen = "NotSeen"
