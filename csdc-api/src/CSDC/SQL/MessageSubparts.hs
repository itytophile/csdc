{-# LANGUAGE OverloadedStrings #-}

module CSDC.SQL.MessageSubparts
  ( sendMessage
  , updateMessage
  , selectSubpart
  , sendReply
  , viewReply
  , select
  , messageReplies
  ) where

import           CSDC.DAO.Types                 ( Message(..)
                                                , MessageInfo(..)
                                                , MessageStatus
                                                , Reply(..)
                                                , ReplyInfo(..)
                                                , Subpart(..)
                                                , Unit(..)
                                                )
import           CSDC.Data.Id                   ( Id(..) )

import qualified CSDC.SQL.Decoder              as Decoder
import qualified CSDC.SQL.Encoder              as Encoder

import           Data.Functor.Contravariant     ( Contravariant(..) )
import           Hasql.Statement                ( Statement(..) )

import qualified Data.ByteString.Char8         as ByteString

viewReply :: Statement (Id (Reply Subpart)) ()
viewReply = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    [ "UPDATE replies_subpart"
    , "SET rstatus = 'Seen' :: reply_status"
    , "WHERE id = $1"
    ]

  encoder = Encoder.id

  decoder = Decoder.noResult

selectSubpart :: Statement (Id (Message Subpart)) (Maybe Subpart)
selectSubpart = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    ["SELECT child, parent", "FROM messages_subpart", "WHERE id = $1"]

  encoder = Encoder.id

  decoder = Decoder.rowMaybe $ Subpart <$> Decoder.id <*> Decoder.id

sendMessage :: Statement (Message Subpart) (Id (Message Subpart))
sendMessage = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    [ "INSERT INTO messages_subpart (mtype, mstatus, message, child, parent)"
    , "VALUES ($1 :: message_type, $2 :: message_status, $3, $4, $5)"
    , "RETURNING id"
    ]

  encoder =
    contramap message_type Encoder.messageType
      <> contramap message_status                   Encoder.messageStatus
      <> contramap message_text                     Encoder.text
      <> contramap (subpart_child . message_value)  Encoder.id
      <> contramap (subpart_parent . message_value) Encoder.id

  decoder = Decoder.singleRow Decoder.id

updateMessage :: Statement (Id (Message Subpart), MessageStatus) ()
updateMessage = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    [ "UPDATE messages_member"
    , "SET mstatus = $2 :: message_status"
    , "WHERE id = $1"
    ]

  encoder = contramap fst Encoder.id <> contramap snd Encoder.messageStatus

  decoder = Decoder.noResult

sendReply :: Statement (Reply Subpart) (Id (Reply Subpart))
sendReply = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    [ "INSERT INTO replies_subpart (rtype, mtype, rstatus, reply, message)"
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

select :: Statement (Id Unit) [(Id (Message Subpart), MessageInfo Subpart)]
select = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    [ "SELECT m.id, m.mtype, m.mstatus, m.message, m.child, m.parent, u1.name, u2.name"
    , "FROM"
    , "  messages_subpart m"
    , "JOIN"
    , "  units u1 ON u1.id = m.child"
    , "JOIN"
    , "  units u2 ON u2.id = m.parent"
    , "WHERE m.child = $1 OR m.parent = $1"
    ]

  encoder = Encoder.id

  makeMessage uid ty tx st p u pn un =
    (uid, MessageInfo ty tx st (Subpart p u) pn un)

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
  :: Statement [Id (Message Subpart)] [(Id (Reply Subpart), ReplyInfo Subpart)]
messageReplies = Statement sql encoder decoder True
 where
  sql = ByteString.unlines
    [ "SELECT r.id, r.rtype, r.mtype, r.reply, r.rstatus, m.mtype, m.mstatus, m.message, m.child, m.parent, u1.name, u2.name"
    , "FROM"
    , "  replies_subpart r"
    , "JOIN"
    , "  messages_subpart m ON m.id = r.message"
    , "JOIN"
    , "  units u1 ON u1.id = m.child"
    , "JOIN"
    , "  units u2 ON u2.id = m.parent"
    , "WHERE r.message = ANY($1)"
    ]

  encoder = Encoder.idList

  makeReply uid ty mty tx st m_ty m_tx m_st m_p m_u m_pn m_un =
    ( uid
    , ReplyInfo ty mty tx st
      $ MessageInfo m_ty m_tx m_st (Subpart m_p m_u) m_pn m_un
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
