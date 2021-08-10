{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module CSDC.DAO.Class
  ( HasDAO(..)
  , HasCRUD(..)
  , HasRelation(..)
  , IsRelation(..)
  , HasMessage(..)
  , getPersonInfo
  , getUnitInfo
  , getUserUnits
  , getUnitMembers
  , getUnitChildren
  , getUnitParents
  ) where

import qualified CSDC.Auth.ORCID               as ORCID
import           CSDC.DAO.Types                 ( Inbox(..)
                                                , Member(..)
                                                , Message(..)
                                                , Person(..)
                                                , PersonInfo(..)
                                                , Reply(..)
                                                , Subpart(..)
                                                , Unit
                                                , UnitInfo(..)
                                                )
import           CSDC.Data.Id                   ( Id
                                                , WithId(..)
                                                )
import           CSDC.Data.IdMap                ( IdMap )
import qualified CSDC.Data.IdMap               as IdMap
import           Control.Monad.Reader           ( ReaderT )
import           Control.Monad.Trans            ( MonadTrans(..) )
import           Data.Traversable               ( forM )

--------------------------------------------------------------------------------
-- CRUD

class Monad m => HasCRUD a m where
  select :: Id a -> m (Maybe a)
  insert :: a -> m (Id a)
  update :: Id a -> a -> m ()
  delete :: Id a -> m ()

--------------------------------------------------------------------------------
-- Relation

class IsRelation r where
  type RelationL r
  type RelationR r
  makeRelation :: Id (RelationL r) -> Id (RelationR r) -> r
  projRelationL :: r -> Id (RelationL r)
  projRelationR :: r -> Id (RelationR r)

instance IsRelation Member where
  type RelationL Member = Person
  type RelationR Member = Unit
  makeRelation  = Member
  projRelationL = member_person
  projRelationR = member_unit

instance IsRelation Subpart where
  type RelationL Subpart = Unit
  type RelationR Subpart = Unit
  makeRelation  = Subpart
  projRelationL = subpart_child
  projRelationR = subpart_parent

class (IsRelation r, Monad m) => HasRelation r m where
  selectRelationL :: Id (RelationL r) -> m (IdMap r r)
  selectRelationR :: Id (RelationR r) -> m (IdMap r r)
  insertRelation :: r -> m (Id r)
  deleteRelation :: Id r -> m ()

--------------------------------------------------------------------------------
-- Message

class HasRelation r m => HasMessage r m where
  sendMessage :: Message r -> m (Id (Message r))
  sendReply :: Reply r -> m (Id (Reply r))
  viewReply :: Id (Reply r) -> m ()

--------------------------------------------------------------------------------
-- DAO

class
  ( HasCRUD Person m,
    HasCRUD Unit m,
    HasRelation Member m,
    HasRelation Subpart m,
    HasMessage Member m,
    HasMessage Subpart m
  ) =>
  HasDAO m
  where
  selectPersonORCID :: ORCID.Id -> m (Maybe (Id Person))

  rootUnit :: m (Id Unit)

  createUnit :: Id Person -> m (WithId Member)

  inboxPerson :: Id Person -> m Inbox

  inboxUnit :: Id Unit -> m Inbox

getPersonInfo :: HasDAO m => Id Person -> m (Maybe PersonInfo)
getPersonInfo uid = select @Person uid >>= \case
  Nothing     -> pure Nothing
  Just person -> do
    membersList <- selectRelationL @Member uid
    pairs       <- forM membersList
      $ \(Member _ unitId) -> fmap (WithId unitId) <$> select @Unit unitId
    case sequence pairs of
      Nothing      -> pure Nothing
      Just members -> pure $ Just PersonInfo { personInfo_id      = uid
                                             , personInfo_person  = person
                                             , personInfo_members = members
                                             }

getUnitInfo :: HasDAO m => Id Unit -> m (Maybe UnitInfo)
getUnitInfo uid = select @Unit uid >>= \case
  Nothing   -> pure Nothing
  Just unit -> do
    members  <- getUnitMembers uid
    children <- getUnitChildren uid
    parents  <- getUnitParents uid
    pure $ Just UnitInfo { unitInfo_id       = uid
                         , unitInfo_unit     = unit
                         , unitInfo_members  = members
                         , unitInfo_children = children
                         , unitInfo_parents  = parents
                         }

getUserUnits :: HasDAO m => Id Person -> m (IdMap Member Unit)
getUserUnits uid = do
  members <- selectRelationL @Member uid
  pairs   <- forM members $ \(Member _ unitId) -> select @Unit unitId
  case sequence pairs of
    Nothing -> pure IdMap.empty
    Just m  -> pure m

getUnitMembers :: HasDAO m => Id Unit -> m (IdMap Member (WithId Person))
getUnitMembers uid = do
  members <- selectRelationR @Member uid
  pairs   <- forM members $ \(Member personId _) ->
    fmap (WithId personId) <$> select @Person personId
  case sequence pairs of
    Nothing -> pure IdMap.empty
    Just m  -> pure m

getUnitChildren :: HasDAO m => Id Unit -> m (IdMap Subpart (WithId Unit))
getUnitChildren uid = do
  subparts <- selectRelationR @Subpart uid
  pairs    <- forM subparts
    $ \(Subpart childId _) -> fmap (WithId childId) <$> select @Unit childId
  case sequence pairs of
    Nothing -> pure IdMap.empty
    Just m  -> pure m

getUnitParents :: HasDAO m => Id Unit -> m (IdMap Subpart (WithId Unit))
getUnitParents uid = do
  subparts <- selectRelationL @Subpart uid
  pairs    <- forM subparts $ \(Subpart _ parentId) ->
    fmap (WithId parentId) <$> select @Unit parentId
  case sequence pairs of
    Nothing -> pure IdMap.empty
    Just m  -> pure m

--------------------------------------------------------------------------------
-- Instances

instance HasMessage a m => HasMessage a (ReaderT r m) where
  sendMessage = lift1 sendMessage
  sendReply   = lift1 sendReply
  viewReply   = lift1 viewReply

instance HasCRUD a m => HasCRUD a (ReaderT r m) where
  select = lift1 select
  insert = lift1 insert
  update = lift2 update
  delete = lift1 delete

instance HasRelation a m => HasRelation a (ReaderT r m) where
  selectRelationL = lift1 selectRelationL
  selectRelationR = lift1 selectRelationR
  insertRelation  = lift1 insertRelation
  deleteRelation  = lift1 deleteRelation

-- | This instance is here for the delegation to @UserT@. It only depends on
-- 'MonadTrans'.
instance HasDAO m => HasDAO (ReaderT r m) where
  selectPersonORCID = lift1 selectPersonORCID
  rootUnit          = lift rootUnit
  createUnit        = lift1 createUnit
  inboxPerson       = lift1 inboxPerson
  inboxUnit         = lift1 inboxUnit

lift1 :: (MonadTrans t, Monad m) => (a -> m b) -> a -> t m b
lift1 f a = lift (f a)

lift2 :: (MonadTrans t, Monad m) => (a -> b -> m c) -> a -> b -> t m c
lift2 f a b = lift (f a b)
