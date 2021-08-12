{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module CSDC.API.DAO (
  API,
  serveAPI,
) where

import qualified CSDC.Auth.ORCID     as ORCID
import           CSDC.DAO.Types
import           CSDC.Prelude
import           Control.Lens
import           Data.Proxy
import           Data.Swagger
import           Data.Swagger.Schema (ToSchema)
import           GHC.Types
import           Servant
--------------------------------------------------------------------------------
-- Synonyms

type GetJSON a = Get '[Servant.JSON] a
type PostJSON a b = ReqBody '[Servant.JSON] a :> Post '[Servant.JSON] b
type DeleteJSON a = Delete '[Servant.JSON] a
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
    :<|> CSDC.Prelude.delete

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
  "root" :> Get '[Servant.JSON] (Id Unit)
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

type SwaggerAPI = "swagger.json" :> Get '[Servant.JSON] Swagger

instance ToParamSchema (Id a)

instance ToSchema ORCID.Id
instance ToSchema Person
instance ToSchema Unit
instance ToSchema Member
instance ToSchema Subpart
instance ToSchema MessageStatus
instance ToSchema MessageType
instance ToSchema ReplyStatus
instance ToSchema ReplyType
instance ToSchema PersonInfo
instance ToSchema Inbox
instance ToSchema UnitInfo

instance ToSchema (Id a)
instance ToSchema a => ToSchema (WithId a)
instance ToSchema a => ToSchema (IdMap b a)

instance ToSchema a => ToSchema (Message a)
instance ToSchema a => ToSchema (Reply a)
instance ToSchema a => ToSchema (MessageInfo a)
instance ToSchema a => ToSchema (ReplyInfo a)


instance ToSchema (User (Id Person)) where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy :: Proxy String)
    intSchema <- declareSchemaRef (Proxy :: Proxy Int)
    return $ NamedSchema (Just "User") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("tag", stringSchema)
          , ("contents", intSchema)
          ]
      & required .~ [ "tag" ]

serveAPI :: (HasUser m, HasDAO m) => ServerT API m
serveAPI =
  servePersonAPI
    :<|> serveUnitAPI
    :<|> serveMemberAPI
    :<|> serveSubpartAPI
    :<|> serveMessageAPI
