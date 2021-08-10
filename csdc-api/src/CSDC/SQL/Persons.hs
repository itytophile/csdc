{-# LANGUAGE OverloadedStrings #-}

module CSDC.SQL.Persons
  ( select
  , insert
  , insertAt
  , update
  , delete
  , selectORCID
  ) where

import CSDC.DAO.Types (Person (..))
import CSDC.Data.Id (Id (..))

import qualified CSDC.Auth.ORCID as ORCID
import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

import qualified Data.ByteString.Char8 as ByteString

select :: Statement (Id Person) (Maybe Person)
select = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT name, description, orcid"
      , "FROM persons"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowMaybe $
      Person <$>
        Decoder.text <*>
        Decoder.text <*>
        Decoder.orcidId

selectORCID :: Statement ORCID.Id (Maybe (Id Person))
selectORCID = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT id"
      , "FROM persons"
      , "WHERE orcid = $1"
      ]

    encoder = Encoder.orcidId

    decoder = Decoder.rowMaybe Decoder.id

insert :: Statement Person (Id Person)
insert = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO persons (name, description, orcid)"
      , "VALUES ($1, $2, $3)"
      , "RETURNING id"
      ]

    encoder =
      contramap person_name Encoder.text <>
      contramap person_description Encoder.text <>
      contramap person_orcid Encoder.orcidId

    decoder = Decoder.singleRow Decoder.id

insertAt :: Statement (Id Person, Person) ()
insertAt = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO persons (id, name, description, orcid)"
      , "VALUES ($1, $2, $3, $4)"
      ]

    encoder =
      contramap fst Encoder.id <>
      contramap (person_name . snd) Encoder.text <>
      contramap (person_description . snd) Encoder.text <>
      contramap (person_orcid . snd) Encoder.orcidId

    decoder = Decoder.noResult

update :: Statement (Id Person, Person) ()
update = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "UPDATE persons"
      , "SET name = $2, description = $3, orcid = $4"
      , "WHERE id = $1"
      ]

    encoder =
      contramap fst Encoder.id <>
      contramap (person_name . snd) Encoder.text <>
      contramap (person_description . snd) Encoder.text <>
      contramap (person_orcid . snd) Encoder.orcidId

    decoder = Decoder.noResult

delete :: Statement (Id Person) ()
delete = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "DELETE FROM persons"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.noResult
