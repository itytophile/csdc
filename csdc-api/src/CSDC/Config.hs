{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module CSDC.Config
  ( -- * Config
    Config (..)
  , readConfig
  , showConfig
    -- * Secret
  , Secret (..)
  , readSecret
    -- * Context
  , Context (..)
  , activate
  ) where

import CSDC.Prelude

import qualified CSDC.Auth as Auth
import qualified CSDC.Auth.Admin as Admin
import qualified CSDC.Auth.ORCID as ORCID
import qualified CSDC.DAO as DAO
import qualified CSDC.SQL as SQL

import Data.Aeson (decodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.ByteString.Lazy.Char8 as ByteString

--------------------------------------------------------------------------------
-- SQL Config

data SQLConfig = SQLConfig SQL.Config | SQLConfigEnv
    deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON SQLConfig

data SQLSecret = SQLSecret SQL.Secret | SQLSecretEnv SQL.Config SQL.Secret
    deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON SQLSecret

getSQLSecret :: IO (Maybe SQLSecret)
getSQLSecret =
  env "DATABASE_URL" >>= \case
    Nothing -> pure Nothing
    Just str -> pure $ do
      (config, secret) <- SQL.parseURL $ Text.unpack str
      pure $ SQLSecretEnv config secret

toSQLContext :: SQLConfig -> SQLSecret -> IO SQL.Context
toSQLContext (SQLConfig config) (SQLSecret secret) =
  SQL.activate config secret
toSQLContext SQLConfigEnv (SQLSecretEnv config secret) =
  SQL.activate config secret
toSQLContext _ _ =
  error "SQL badly configured."

--------------------------------------------------------------------------------
-- Config

data Config = Config
  { config_port :: Int
  , config_path :: FilePath
  , config_orcidEndpoint :: ORCID.Endpoint
  , config_sql :: SQLConfig
  , config_migration :: FilePath
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Config

readConfig :: MonadIO m => FilePath -> m (Maybe Config)
readConfig path = liftIO $ do
  mconfig <- decodeFileStrict path
  mport <- lookupEnv "PORT"
  case readMaybe =<< mport of
    Nothing ->
      pure mconfig
    Just port ->
      case mconfig of
        Nothing ->
          pure Nothing
        Just config ->
          pure $ Just config { config_port = port }

showConfig :: MonadIO m => Config -> m ()
showConfig config =
  let
    str = ByteString.unpack $ encodePretty config
  in
    liftIO $ putStrLn str

--------------------------------------------------------------------------------
-- Secret

data Secret = Secret
  { secret_token :: Text
  , secret_orcidId :: Text
  , secret_orcidSecret :: Text
  , secret_sql :: SQLSecret
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Secret

-- | Read secrets either from a file, or from environment variables.
readSecret :: MonadIO m => Maybe FilePath -> m (Maybe Secret)
readSecret (Just path) = liftIO $ decodeFileStrict path
readSecret Nothing = liftIO $ do
  mToken <- env "SECRET_TOKEN"
  mOrcidId <- env "SECRET_ORCID_ID"
  mOrcidSecret <- env "SECRET_ORCID_SECRET"
  mSql <- getSQLSecret
  pure $ Secret <$> mToken <*> mOrcidId <*> mOrcidSecret <*> mSql

--------------------------------------------------------------------------------
-- Context

data Context = Context
  { context_port :: Int
  , context_path :: FilePath
  , context_auth :: Auth.Config
  , context_dao :: DAO.Context
  , context_migration :: FilePath
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Context

activate :: Config -> Secret -> IO Context
activate config secret = do
  sql <- toSQLContext (config_sql config) (secret_sql secret)
  pure Context
    { context_port = config_port config
    , context_path = config_path config
    , context_auth = Auth.Config
        { Auth.config_orcid = ORCID.Config
            { ORCID.config_id = secret_orcidId secret
            , ORCID.config_secret = secret_orcidSecret secret
            , ORCID.config_endpoint = config_orcidEndpoint config
            }
        , Auth.config_admin = Admin.Token
            { Admin.token_value =
                Text.Encoding.encodeUtf8 (secret_token secret)
            }
        }
    , context_dao = DAO.Context
        { DAO.context_sql = sql
        }
    , context_migration = config_migration config
    }

--------------------------------------------------------------------------------
-- Helper

env :: String -> IO (Maybe Text)
env var = fmap Text.pack <$> lookupEnv var
