{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module CSDC.Data.IdMap
  ( IdMap (..),
    IdMap',
    empty,
    lookup,
    get,
    find,
    insert,
    insertNew,
    update,
    delete,
    filter,
    keys,
    fromList,
    fromWithIds,
  )
where

import CSDC.Data.Id (Id (..), WithId (..), next, zero)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import Prelude hiding (filter, lookup)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Type definition

newtype IdMap a b = IdMap {getIdMap :: IntMap b}
  deriving (Show, Generic, Eq, Functor, Foldable, ToJSON, FromJSON)
  deriving stock (Traversable)

type IdMap' a = IdMap a a

empty :: IdMap a b
empty = IdMap IntMap.empty

lookup :: Id a -> IdMap a b -> Maybe b
lookup (Id uid) (IdMap m) = IntMap.lookup uid m

get :: Id a -> IdMap a b -> b
get (Id uid) (IdMap m) = m IntMap.! uid

find :: (b -> Bool) -> IdMap a b -> Maybe (Id a, b)
find p (IdMap m) =
  coerce $
    List.find (p . snd) $
      IntMap.toList m

insert :: Id a -> b -> IdMap a b -> IdMap a b
insert (Id uid) a (IdMap m) = IdMap $ IntMap.insert uid a m

update :: Id a -> (b -> b) -> IdMap a b -> IdMap a b
update (Id uid) f (IdMap m) = IdMap $ IntMap.adjust f uid m

insertNew :: b -> IdMap a b -> (Id a, IdMap a b)
insertNew a idmap@(IdMap m) =
  let uid =
        case IntMap.lookupMax m of
          Nothing -> zero
          Just (uid', _) -> next (Id uid')
   in (uid, insert uid a idmap)

delete :: Id a -> IdMap a b -> IdMap a b
delete (Id uid) (IdMap m) = IdMap $ IntMap.delete uid m

filter :: (b -> Bool) -> IdMap a b -> IdMap a b
filter f (IdMap m) = IdMap $ IntMap.filter f m

keys :: IdMap a b -> [Id a]
keys (IdMap m) = Id <$> IntMap.keys m

fromList :: [(Id a, b)] -> IdMap a b
fromList = IdMap . IntMap.fromList . coerce

fromWithIds :: [WithId a] -> IdMap a a
fromWithIds = fromList . fmap (\(WithId i a) -> (i, a))
