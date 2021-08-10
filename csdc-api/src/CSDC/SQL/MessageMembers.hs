{-# LANGUAGE OverloadedStrings #-}

module CSDC.SQL.MessageMembers
  ( sendMessage
  , selectMember
  , updateMessage
  , sendReply
  , viewReply
  , Filter(..)
  , select
  , messageReplies
  ) where

import           CSDC.DAO.Types                 ( Member(..)
                                                , Message(..)
                                                , MessageInfo(..)
                                                , MessageStatus
                                                , Person
                                                , Reply(..)
                                                , ReplyInfo(..)
                                                , Unit
                                                )
import           CSDC.Data.Id                   ( Id(..) )

import qualified CSDC.SQL.Decoder              as Decoder
import qualified CSDC.SQL.Encoder              as Encoder

import           Data.Functor.Contravariant     ( Contravariant(..) )
import           Hasql.Statement                ( Statement(..) )

import qualified Data.ByteString.Char8         as ByteString

viewReply :: Statement (Id (Reply Member)) ()
viewReply = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    [ "UPDATE replies_member"
    , "SET rstatus = 'Seen' :: reply_status"
    , "WHERE id = $1"
    ]

  encoder = Encoder.id

  decoder = Decoder.noResult

selectMember :: Statement (Id (Message Member)) (Maybe Member)
selectMember = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    ["SELECT person, unit", "FROM messages_member", "WHERE id = $1"]

  encoder = Encoder.id

  decoder = Decoder.rowMaybe $ Member <$> Decoder.id <*> Decoder.id

sendMessage :: Statement (Message Member) (Id (Message Member))
sendMessage = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    [ "INSERT INTO messages_member (mtype, mstatus, message, person, unit)"
    , "VALUES ($1 :: message_type, $2 :: message_status, $3, $4, $5)"
    , "RETURNING id"
    ]

  encoder =
    contramap message_type Encoder.messageType
      <> contramap message_status                  Encoder.messageStatus
      <> contramap message_text                    Encoder.text
      <> contramap (member_person . message_value) Encoder.id
      <> contramap (member_unit . message_value)   Encoder.id

  decoder = Decoder.singleRow Decoder.id

updateMessage :: Statement (Id (Message Member), MessageStatus) ()
updateMessage = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    [ "UPDATE messages_member"
    , "SET mstatus = $2 :: message_status"
    , "WHERE id = $1"
    ]

  encoder = contramap fst Encoder.id <> contramap snd Encoder.messageStatus

  decoder = Decoder.noResult


sendReply :: Statement (Reply Member) (Id (Reply Member))
sendReply = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    [ "INSERT INTO replies_member (rtype, mtype, rstatus, reply, message)"
    , "VALUES ($1 :: reply_type, $2 :: message_type, $3 :: reply_status, $4, $5)"
    , "RETURNING id"
    ]

  encoder =
    contramap reply_type Encoder.replyType
      <> contramap reply_mtype  Encoder.messageType
      <> contramap reply_status Encoder.replyStatus
      <> contramap reply_text   Encoder.text
      <> contramap reply_id     Encoder.id

  decoder = Decoder.singleRow Decoder.id

data Filter = Filter
  { filter_person :: Maybe (Id Person)
  , filter_unit   :: Maybe (Id Unit)
  }

select :: Statement Filter [(Id (Message Member), MessageInfo Member)]
select = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    [ "SELECT m.id, m.mtype, m.mstatus, m.message, m.person, m.unit, p.name, u.name"
    , "FROM"
    , "  messages_member m"
    , "JOIN"
    , "  persons p ON p.id = m.person"
    , "JOIN"
    , "  units u ON u.id = m.unit"
    , "WHERE"
    , "  COALESCE (person = $1, TRUE)"
    , "AND"
    , "  COALESCE (unit = $2, TRUE)"
    ]

  encoder =
    contramap filter_person Encoder.idNullable
      <> contramap filter_unit Encoder.idNullable

  makeMessage uid ty tx st p u pn un =
    (uid, MessageInfo ty tx st (Member p u) pn un)

  decoder =
    Decoder.rowList
      $   makeMessage
      <$> Decoder.id
      <*> Decoder.messageType
      <*> Decoder.messageStatus
      <*> Decoder.text
      <*> Decoder.id
      <*> Decoder.id
      <*> Decoder.text
      <*> Decoder.text

messageReplies
  :: Statement [Id (Message Member)] [(Id (Reply Member), ReplyInfo Member)]
messageReplies = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    [ "SELECT r.id, r.rtype, r.mtype, r.reply, r.rstatus, m.mtype, m.mstatus, m.message, m.person, m.unit, p.name, u.name"
    , "FROM"
    , "  replies_member r"
    , "JOIN"
    , "  messages_member m ON m.id = r.message"
    , "JOIN"
    , "  persons p ON p.id = m.person"
    , "JOIN"
    , "  units u ON u.id = m.unit"
    , "WHERE r.message = ANY($1)"
    ]

  encoder = Encoder.idList

  makeReply uid ty mty tx st m_ty m_tx m_st m_p m_u m_pn m_un =
    ( uid
    , ReplyInfo ty mty tx st
      $ MessageInfo m_ty m_tx m_st (Member m_p m_u) m_pn m_un
    )

  decoder =
    Decoder.rowList
      $   makeReply
      <$> Decoder.id
      <*> Decoder.replyType
      <*> Decoder.messageType
      <*> Decoder.text
      <*> Decoder.replyStatus
      <*> Decoder.messageType
      <*> Decoder.messageStatus
      <*> Decoder.text
      <*> Decoder.id
      <*> Decoder.id
      <*> Decoder.text
      <*> Decoder.text
