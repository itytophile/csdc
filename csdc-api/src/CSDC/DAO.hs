{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CSDC.DAO where

import qualified CSDC.Auth.ORCID               as ORCID
import qualified CSDC.Data.IdMap               as IdMap
import           CSDC.Prelude                   ( FromJSON
                                                , Generic
                                                , HasCRUD(..)
                                                , HasDAO(..)
                                                , HasMessage(..)
                                                , HasRelation(..)
                                                , Id(Id)
                                                , Inbox
                                                  ( Inbox
                                                  , inbox_messageMember
                                                  , inbox_messageSubpart
                                                  , inbox_replyMember
                                                  , inbox_replySubpart
                                                  )
                                                , JSON(..)
                                                , Member(..)
                                                , MessageInfo
                                                  ( messageInfo_status
                                                  , messageInfo_type
                                                  )
                                                , MessageStatus
                                                  ( Accepted
                                                  , Rejected
                                                  , Waiting
                                                  )
                                                , MessageType
                                                  ( Invitation
                                                  , Submission
                                                  )
                                                , MonadIO(..)
                                                , Person(Person)
                                                , Reply(reply_id, reply_type)
                                                , ReplyInfo
                                                  ( replyInfo_mtype
                                                  , replyInfo_status
                                                  )
                                                , ReplyStatus(NotSeen)
                                                , ReplyType(Accept, Reject)
                                                , Subpart
                                                , ToJSON
                                                , Unit(..)
                                                , WithId(WithId)
                                                )
import qualified CSDC.SQL                      as SQL
import qualified CSDC.SQL.Members              as SQL.Members
import qualified CSDC.SQL.MessageMembers       as SQL.MessageMembers
import qualified CSDC.SQL.MessageSubparts      as SQL.MessageSubparts
import qualified CSDC.SQL.Persons              as SQL.Persons
import qualified CSDC.SQL.Subparts             as SQL.Subparts
import qualified CSDC.SQL.Units                as SQL.Units
import           Control.Exception              ( Exception
                                                , throwIO
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                , asks
                                                )

--------------------------------------------------------------------------------
-- Context

newtype Context = Context {context_sql :: SQL.Context}
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON Context

--------------------------------------------------------------------------------
-- Error

newtype Error
  = ErrorSQL SQL.Error
  deriving (Show, Eq)

instance Exception Error

--------------------------------------------------------------------------------
-- Action

newtype Action a = Action (ReaderT Context IO a)
  deriving (Functor, Applicative, Monad, MonadReader Context, MonadIO)

run :: MonadIO m => Context -> Action a -> m a
run ctx (Action act) = liftIO $ runReaderT act ctx

check :: Action ()
check = do
  select rootUnitId >>= \case
    Nothing -> do
      let person = Person "President" "" (ORCID.Id "")
      runSQL $ SQL.query SQL.Persons.insertAt (rootPersonId, person)
      let unit = Unit "CSDC" "" rootPersonId
      runSQL $ SQL.query SQL.Units.insertAt (rootUnitId, unit)
      let member = Member rootPersonId rootUnitId
      _ <- runSQL $ SQL.query SQL.Members.insert member
      pure ()
    Just _ -> pure ()

runSQL :: SQL.Action a -> Action a
runSQL act = do
  ctx <- asks context_sql
  SQL.run ctx act >>= \case
    Left  e -> liftIO $ throwIO $ ErrorSQL e
    Right a -> pure a

--------------------------------------------------------------------------------
-- Capabilities

instance HasCRUD Person Action where
  select i = runSQL $ SQL.query SQL.Persons.select i
  insert p = runSQL $ SQL.query SQL.Persons.insert p
  update i p = runSQL $ SQL.query SQL.Persons.update (i, p)
  delete i = runSQL $ SQL.query SQL.Persons.delete i

instance HasCRUD Unit Action where
  select i = runSQL $ SQL.query SQL.Units.select i
  insert p = runSQL $ SQL.query SQL.Units.insert p
  update i p = runSQL $ SQL.query SQL.Units.update (i, p)
  delete i = runSQL $ SQL.query SQL.Units.delete i

instance HasRelation Member Action where
  selectRelationL i = runSQL $ SQL.query SQL.Members.selectL i
  selectRelationR i = runSQL $ SQL.query SQL.Members.selectR i
  insertRelation r = runSQL $ SQL.query SQL.Members.insert r
  deleteRelation i = runSQL $ SQL.query SQL.Members.delete i

instance HasRelation Subpart Action where
  selectRelationL i = runSQL $ SQL.query SQL.Subparts.selectL i
  selectRelationR i = runSQL $ SQL.query SQL.Subparts.selectR i
  insertRelation r = runSQL $ SQL.query SQL.Subparts.insert r
  deleteRelation i = runSQL $ SQL.query SQL.Subparts.delete i

instance HasMessage Member Action where
  sendMessage m = runSQL $ SQL.query SQL.MessageMembers.sendMessage m

  sendReply r = do
    rid <- runSQL $ SQL.query SQL.MessageMembers.sendReply r
    let status = case reply_type r of
          Accept -> Accepted
          Reject -> Rejected
        uid = reply_id r
    runSQL $ SQL.query SQL.MessageMembers.updateMessage (uid, status)
    case reply_type r of
      Accept ->
        runSQL (SQL.query SQL.MessageMembers.selectMember uid) >>= \case
          Nothing  -> pure ()
          Just val -> do
            _ <- insertRelation val
            pure ()
      _ -> pure ()
    pure rid

  viewReply i = runSQL $ SQL.query SQL.MessageMembers.viewReply i

instance HasMessage Subpart Action where
  sendMessage m = runSQL $ SQL.query SQL.MessageSubparts.sendMessage m

  sendReply r = do
    rid <- runSQL $ SQL.query SQL.MessageSubparts.sendReply r
    let status = case reply_type r of
          Accept -> Accepted
          Reject -> Rejected
        uid = reply_id r
    runSQL $ SQL.query SQL.MessageSubparts.updateMessage (uid, status)
    case reply_type r of
      Accept ->
        runSQL (SQL.query SQL.MessageSubparts.selectSubpart uid) >>= \case
          Nothing  -> pure ()
          Just val -> do
            _ <- insertRelation val
            pure ()
      _ -> pure ()
    pure rid

  viewReply i = runSQL $ SQL.query SQL.MessageSubparts.viewReply i

instance HasDAO Action where
  selectPersonORCID i = runSQL $ SQL.query SQL.Persons.selectORCID i

  rootUnit = pure rootUnitId

  createUnit pid = do
    let unit = Unit { unit_name        = "New unit"
                    , unit_description = "New unit description"
                    , unit_chair       = pid
                    }
    uid <- insert unit
    let member = Member { member_person = pid, member_unit = uid }
    mid <- insertRelation member
    return $ WithId mid member

  inboxPerson pid = do
    messagesAll <-
      runSQL $ SQL.query SQL.MessageMembers.select $ SQL.MessageMembers.Filter
        (Just pid)
        Nothing
    let mids = fmap fst messagesAll
    repliesAll <- runSQL $ SQL.query SQL.MessageMembers.messageReplies mids
    let
      predMessage m =
        messageInfo_status m == Waiting && messageInfo_type m == Invitation

      messageMember = IdMap.filter predMessage $ IdMap.fromList messagesAll

      predReply r =
        replyInfo_status r == NotSeen && replyInfo_mtype r == Submission

      replyMember = IdMap.filter predReply $ IdMap.fromList repliesAll

    pure Inbox { inbox_messageMember  = messageMember
               , inbox_replyMember    = replyMember
               , inbox_messageSubpart = IdMap.empty
               , inbox_replySubpart   = IdMap.empty
               }

  inboxUnit uid = do
    messagesSubpartAll <- runSQL $ SQL.query SQL.MessageSubparts.select uid

    repliesSubpartAll  <-
      runSQL $ SQL.query SQL.MessageSubparts.messageReplies $ fmap
        fst
        messagesSubpartAll

    let predMessageSubpart m = messageInfo_status m == Waiting

        messagesSubpart =
          IdMap.filter predMessageSubpart $ IdMap.fromList messagesSubpartAll

        predReplySubpart r = replyInfo_status r == NotSeen

        repliesSubpart =
          IdMap.filter predReplySubpart $ IdMap.fromList repliesSubpartAll

    messagesMemberAll <-
      runSQL $ SQL.query SQL.MessageMembers.select $ SQL.MessageMembers.Filter
        Nothing
        (Just uid)

    repliesMemberAll <-
      runSQL $ SQL.query SQL.MessageMembers.messageReplies $ fmap
        fst
        messagesMemberAll

    let
      predMessageMember m =
        messageInfo_status m == Waiting && messageInfo_type m == Submission

      messageMember =
        IdMap.filter predMessageMember $ IdMap.fromList messagesMemberAll

      predReplyMember r =
        replyInfo_status r == NotSeen && replyInfo_mtype r == Invitation

      replyMember =
        IdMap.filter predReplyMember $ IdMap.fromList repliesMemberAll

    pure Inbox { inbox_messageMember  = messageMember
               , inbox_replyMember    = replyMember
               , inbox_messageSubpart = messagesSubpart
               , inbox_replySubpart   = repliesSubpart
               }

--------------------------------------------------------------------------------
-- Helpers

rootUnitId :: Id Unit
rootUnitId = Id 0

rootPersonId :: Id Person
rootPersonId = Id 0
