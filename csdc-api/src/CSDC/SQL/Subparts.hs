{-# LANGUAGE OverloadedStrings #-}

module CSDC.SQL.Subparts
  ( selectL
  , selectR
  , insert
  , delete
  ) where

import           CSDC.DAO.Types                 ( Subpart(..)
                                                , Unit
                                                )
import           CSDC.Data.Id                   ( Id(..) )
import           CSDC.Data.IdMap                ( IdMap' )

import qualified CSDC.Data.IdMap               as IdMap
import qualified CSDC.SQL.Decoder              as Decoder
import qualified CSDC.SQL.Encoder              as Encoder

import           Data.Functor.Contravariant     ( Contravariant(..) )
import           Hasql.Statement                ( Statement(..) )

import qualified Data.ByteString.Char8         as ByteString

selectL :: Statement (Id Unit) (IdMap' Subpart)
selectL = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    ["SELECT id, child, parent", "FROM subparts", "WHERE child = $1"]

  encoder = Encoder.id

  pair uid person unit = (uid, Subpart person unit)

  decoder =
    fmap IdMap.fromList
      $   Decoder.rowList
      $   pair
      <$> Decoder.id
      <*> Decoder.id
      <*> Decoder.id

selectR :: Statement (Id Unit) (IdMap' Subpart)
selectR = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    ["SELECT id, child, parent", "FROM subparts", "WHERE parent = $1"]

  encoder = Encoder.id

  pair uid child parent = (uid, Subpart child parent)

  decoder =
    fmap IdMap.fromList
      $   Decoder.rowList
      $   pair
      <$> Decoder.id
      <*> Decoder.id
      <*> Decoder.id

insert :: Statement Subpart (Id Subpart)
insert = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    ["INSERT INTO subparts (child, parent)", "VALUES ($1, $2)", "RETURNING id"]

  encoder =
    contramap subpart_child Encoder.id <> contramap subpart_parent Encoder.id

  decoder = Decoder.singleRow Decoder.id

delete :: Statement (Id Subpart) ()
delete = Statement sql encoder decoder True
 where
  sql     = ByteString.unlines ["DELETE FROM subparts", "WHERE id = $1"]

  encoder = Encoder.id

  decoder = Decoder.noResult
