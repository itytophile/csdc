{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module CSDC.DAO.Types
  (
  -- Entities
    Person(..)
  , Unit(..)
  ,
  -- Relations
    Member(..)
  , Subpart(..)
  ,
  -- Messages
    Message(..)
  , MessageStatus(..)
  , MessageType(..)
  , Reply(..)
  , ReplyStatus(..)
  , ReplyType(..)
  ,
  -- GUI Types
    PersonInfo(..)
  , UnitInfo(..)
  , Inbox(..)
  , MessageInfo(..)
  , ReplyInfo(..)
  ) where

import           CSDC.Aeson                     ( JSON(..) )
import qualified CSDC.Auth.ORCID               as ORCID
import           CSDC.Data.Id                   ( Id
                                                , WithId
                                                )
import           CSDC.Data.IdMap                ( IdMap )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

--------------------------------------------------------------------------------
-- Fundamental types

data Person = Person
  { person_name        :: Text
  , person_description :: Text
  , person_orcid       :: ORCID.Id
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON Person

data Unit = Unit
  { unit_name        :: Text
  , unit_description :: Text
  , unit_chair       :: Id Person
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON Unit

--------------------------------------------------------------------------------
-- Fundamental relations

data Member = Member
  { member_person :: Id Person
  , member_unit   :: Id Unit
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON Member

data Subpart = Subpart
  { subpart_child  :: Id Unit
  , subpart_parent :: Id Unit
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON Subpart

--------------------------------------------------------------------------------
-- Messages

data MessageStatus = Waiting | Accepted | Rejected
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via JSON MessageStatus

data MessageType = Invitation | Submission
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via JSON MessageType

data Message a = Message
  { message_type   :: MessageType
  , message_text   :: Text
  , message_status :: MessageStatus
  , message_value  :: a
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON (Message a)

data ReplyStatus = Seen | NotSeen
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via JSON ReplyStatus

data ReplyType = Accept | Reject
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via JSON ReplyType

data Reply a = Reply
  { reply_type   :: ReplyType
  , reply_mtype  :: MessageType
  , reply_text   :: Text
  , reply_status :: ReplyStatus
  , reply_id     :: Id (Message a)
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON (Reply a)

--------------------------------------------------------------------------------
-- GUI Types

data PersonInfo = PersonInfo
  { personInfo_id      :: Id Person
  , personInfo_person  :: Person
  , personInfo_members :: IdMap Member (WithId Unit)
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON PersonInfo

data UnitInfo = UnitInfo
  { unitInfo_id       :: Id Unit
  , unitInfo_unit     :: Unit
  , unitInfo_members  :: IdMap Member (WithId Person)
  , unitInfo_children :: IdMap Subpart (WithId Unit)
  , unitInfo_parents  :: IdMap Subpart (WithId Unit)
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON UnitInfo

data MessageInfo a = MessageInfo
  { messageInfo_type   :: MessageType
  , messageInfo_status :: MessageStatus
  , messageInfo_text   :: Text
  , messageInfo_value  :: a
  , messageInfo_left   :: Text
  , messageInfo_right  :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON (MessageInfo a)

data ReplyInfo a = ReplyInfo
  { replyInfo_type    :: ReplyType
  , replyInfo_mtype   :: MessageType
  , replyInfo_text    :: Text
  , replyInfo_status  :: ReplyStatus
  , replyInfo_message :: MessageInfo a
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON (ReplyInfo a)

data Inbox = Inbox
  { inbox_messageMember  :: IdMap (Message Member) (MessageInfo Member)
  , inbox_replyMember    :: IdMap (Reply Member) (ReplyInfo Member)
  , inbox_messageSubpart :: IdMap (Message Subpart) (MessageInfo Subpart)
  , inbox_replySubpart   :: IdMap (Reply Subpart) (ReplyInfo Subpart)
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON Inbox
