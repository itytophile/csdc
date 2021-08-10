{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module CSDC.API.DAO (
  API,
  serveAPI,
) where

import CSDC.Prelude
    ( getPersonInfo,
      getUnitChildren,
      getUnitInfo,
      getUnitMembers,
      getUnitParents,
      getUserUnits,
      HasCRUD(..),
      HasDAO(inboxUnit, rootUnit, createUnit, inboxPerson),
      HasMessage(..),
      HasRelation(..),
      IsRelation(RelationL, RelationR),
      Inbox,
      Member,
      Message,
      Person,
      PersonInfo,
      Reply,
      Subpart,
      Unit,
      UnitInfo,
      Id,
      WithId,
      IdMap,
      HasUser(..),
      UserId )

import GHC.Types (Symbol)
import Servant
    ( type (:<|>)(..),
      Capture,
      JSON,
      ReqBody,
      type (:>),
      Delete,
      Get,
      Post,
      HasServer(ServerT) )

--------------------------------------------------------------------------------
-- Synonyms

type GetJSON a = Get '[JSON] a
type PostJSON a b = ReqBody '[JSON] a :> Post '[JSON] b
type DeleteJSON a = Delete '[JSON] a
type CaptureId a = Capture "id" (Id a)

--------------------------------------------------------------------------------
-- CRUD, REL and MSG API

type CRUD a =
  CaptureId a :> GetJSON (Maybe a)
    :<|> PostJSON a (Id a)
    :<|> CaptureId a :> PostJSON a ()
    :<|> CaptureId a :> DeleteJSON ()

serveCRUD :: HasCRUD a m => ServerT (CRUD a) m
serveCRUD =
  select
    :<|> insert
    :<|> update
    :<|> delete

type REL (left :: Symbol) (right :: Symbol) r =
  left :> CaptureId (RelationL r) :> GetJSON (IdMap r r)
    :<|> right :> CaptureId (RelationR r) :> GetJSON (IdMap r r)
    :<|> PostJSON r (Id r)
    :<|> CaptureId r :> DeleteJSON ()

serveREL ::
  (IsRelation r, HasRelation r m) => ServerT (REL left right r) m
serveREL =
  selectRelationL
    :<|> selectRelationR
    :<|> insertRelation
    :<|> deleteRelation

type MSG r =
  "send" :> PostJSON (Message r) (Id (Message r))
    :<|> "reply" :> PostJSON (Reply r) (Id (Reply r))
    :<|> "view" :> PostJSON (Id (Reply r)) ()

serveMSG :: (Show r, HasMessage r m) => ServerT (MSG r) m
serveMSG =
  sendMessage
    :<|> sendReply
    :<|> viewReply

--------------------------------------------------------------------------------
-- Person API

type PersonAPI =
  "root" :> GetJSON UserId
    :<|> CaptureId Person :> "info" :> GetJSON (Maybe PersonInfo)
    :<|> CaptureId Person :> "units" :> GetJSON (IdMap Member Unit)
    :<|> CRUD Person

servePersonAPI :: (HasUser m, HasDAO m) => ServerT PersonAPI m
servePersonAPI =
  getUser
    :<|> getPersonInfo
    :<|> getUserUnits
    :<|> serveCRUD

--------------------------------------------------------------------------------
-- Unit API

type UnitAPI =
  "root" :> Get '[JSON] (Id Unit)
    :<|> CaptureId Unit :> "info" :> GetJSON (Maybe UnitInfo)
    :<|> CaptureId Unit :> "members" :> GetJSON (IdMap Member (WithId Person))
    :<|> CaptureId Unit :> "children" :> GetJSON (IdMap Subpart (WithId Unit))
    :<|> CaptureId Unit :> "parents" :> GetJSON (IdMap Subpart (WithId Unit))
    :<|> "create" :> PostJSON (Id Person) (WithId Member)
    :<|> CRUD Unit

serveUnitAPI :: HasDAO m => ServerT UnitAPI m
serveUnitAPI =
  rootUnit
    :<|> getUnitInfo
    :<|> getUnitMembers
    :<|> getUnitChildren
    :<|> getUnitParents
    :<|> createUnit
    :<|> serveCRUD

--------------------------------------------------------------------------------
-- Member API

type MemberAPI = REL "person" "unit" Member

serveMemberAPI :: HasDAO m => ServerT MemberAPI m
serveMemberAPI = serveREL

--------------------------------------------------------------------------------
-- Subpart API

type SubpartAPI = REL "child" "parent" Subpart

serveSubpartAPI :: HasDAO m => ServerT SubpartAPI m
serveSubpartAPI = serveREL

--------------------------------------------------------------------------------
-- Message API

type MessageAPI =
  "member" :> MSG Member
    :<|> "subpart" :> MSG Subpart
    :<|> "inbox" :> "person" :> CaptureId Person :> GetJSON Inbox
    :<|> "inbox" :> "unit" :> CaptureId Unit :> GetJSON Inbox

serveMessageAPI :: HasDAO m => ServerT MessageAPI m
serveMessageAPI =
  serveMSG
    :<|> serveMSG
    :<|> inboxPerson
    :<|> inboxUnit

--------------------------------------------------------------------------------
-- API

type API =
  "person" :> PersonAPI
    :<|> "unit" :> UnitAPI
    :<|> "member" :> MemberAPI
    :<|> "subpart" :> SubpartAPI
    :<|> "message" :> MessageAPI

serveAPI :: (HasUser m, HasDAO m) => ServerT API m
serveAPI =
  servePersonAPI
    :<|> serveUnitAPI
    :<|> serveMemberAPI
    :<|> serveSubpartAPI
    :<|> serveMessageAPI
