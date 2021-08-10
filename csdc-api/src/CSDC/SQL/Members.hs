{-# LANGUAGE OverloadedStrings #-}

module CSDC.SQL.Members
  ( selectL
  , selectR
  , insert
  , delete
  ) where

import           CSDC.DAO.Types                 ( Member(..)
                                                , Person
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

selectL :: Statement (Id Person) (IdMap' Member)
selectL = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    ["SELECT id, person, unit", "FROM members", "WHERE person = $1"]

  encoder = Encoder.id

  pair uid person unit = (uid, Member person unit)

  decoder =
    fmap IdMap.fromList
      $   Decoder.rowList
      $   pair
      <$> Decoder.id
      <*> Decoder.id
      <*> Decoder.id

selectR :: Statement (Id Unit) (IdMap' Member)
selectR = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    ["SELECT id, person, unit", "FROM members", "WHERE unit = $1"]

  encoder = Encoder.id

  pair uid child parent = (uid, Member child parent)

  decoder =
    fmap IdMap.fromList
      $   Decoder.rowList
      $   pair
      <$> Decoder.id
      <*> Decoder.id
      <*> Decoder.id

insert :: Statement Member (Id Member)
insert = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    ["INSERT INTO members (person, unit)", "VALUES ($1, $2)", "RETURNING id"]

  encoder =
    contramap member_person Encoder.id <> contramap member_unit Encoder.id

  decoder = Decoder.singleRow Decoder.id

delete :: Statement (Id Member) ()
delete = Statement sql encoder decoder True
 where
  sql     = ByteString.unlines ["DELETE FROM members", "WHERE id = $1"]

  encoder = Encoder.id

  decoder = Decoder.noResult
