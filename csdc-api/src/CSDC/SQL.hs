{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CSDC.SQL
  (
  -- * Config and Secret
    Config(..)
  , Secret(..)
  , parseURL
  ,

  -- * Context
    Context
  , activate
  ,

  -- * Error
    Error(..)
  ,

  -- * Action
    Action(..)
  , run
  , query
  ,

  -- * Migration
    migrate
  ) where

import           CSDC.Prelude
import           Control.Exception              ( Exception
                                                , finally
                                                , throwIO
                                                , try
                                                )
import           Control.Monad                  ( forM_ )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text.Encoding
import           Hasql.Connection               ( Connection
                                                , ConnectionError
                                                )
import qualified Hasql.Connection              as Connection
import qualified Hasql.Migration               as Migration
import           Hasql.Session                  ( QueryError )
import qualified Hasql.Session                 as Session
import           Hasql.Statement                ( Statement )
import qualified Hasql.Transaction.Sessions    as Transaction
import           Network.URI                    ( URI(..)
                                                , URIAuth(..)
                                                , parseURI
                                                )
import           Text.Read                      ( readMaybe )

--------------------------------------------------------------------------------
-- Config and Secret

-- | The configuration of the PostgreSQL server.
data Config = Config
  { config_host     :: Text
  , config_port     :: Int
  , config_user     :: Text
  , config_database :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON Config

-- | The secrets of the PostgreSQL server.
newtype Secret = Secret {secret_password :: Text}
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON Secret

parseURL :: String -> Maybe (Config, Secret)
parseURL txt = do
  uri  <- parseURI txt
  auth <- uriAuthority uri
  port <- readMaybe $ tail $ uriPort auth
  let (user, pwd) = span (/= ':') $ uriUserInfo auth
      password    = init $ tail pwd
  pure
    ( Config { config_host     = Text.pack $ uriRegName auth
             , config_port     = port
             , config_user     = Text.pack user
             , config_database = Text.pack $ tail $ uriPath uri
             }
    , Secret { secret_password = Text.pack password }
    )

--------------------------------------------------------------------------------
-- Context

-- | The context of the PostgreSQL server.
data Context = Context
  { context_config :: Config
  , context_secret :: Secret
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSON Context

{- | Activate the configuration.
 TODO: Check the validity of the configuration.
-}
activate :: Config -> Secret -> IO Context
activate config secret = pure $ Context config secret

--------------------------------------------------------------------------------
-- Error

-- | The errors that can be emitted from SQL actions.
data Error
  = ErrorConnection ConnectionError
  | ErrorQuery QueryError
  | ErrorMigration Migration.MigrationError
  deriving (Show, Eq)

instance Exception Error

--------------------------------------------------------------------------------
-- Action

-- | An action performing SQL operations.
newtype Action a = Action (ReaderT Connection IO a)
  deriving (Functor, Applicative, Monad)

-- | Run a SQL action and return possible errors.
run :: MonadIO m => Context -> Action a -> m (Either Error a)
run (Context config secret) (Action m) = liftIO $ do
  let settings = Connection.settings
        (Text.Encoding.encodeUtf8 $ config_host config)
        (fromIntegral $ config_port config)
        (Text.Encoding.encodeUtf8 $ config_user config)
        (Text.Encoding.encodeUtf8 $ secret_password secret)
        (Text.Encoding.encodeUtf8 $ config_database config)

  Connection.acquire settings >>= \case
    Left  err  -> pure $ Left $ ErrorConnection err
    Right conn -> try (runReaderT m conn) `finally` Connection.release conn

-- | Lift a SQL statement into an action.
query :: Statement a b -> a -> Action b
query stm a = Action $ do
  conn <- ask
  let session = Session.statement a stm
  liftIO (Session.run session conn) >>= \case
    Left  err -> liftIO $ throwIO $ ErrorQuery err
    Right res -> pure res

--------------------------------------------------------------------------------
-- Migration

migrate :: FilePath -> Action ()
migrate path = Action $ do
  commands <- liftIO $ Migration.loadMigrationsFromDirectory path
  let
    transactions =
      fmap Migration.runMigration $ Migration.MigrationInitialization : commands
    toSession =
      Transaction.transaction Transaction.ReadCommitted Transaction.Write
    sessions = fmap toSession transactions
  conn <- ask
  liftIO $ forM_ sessions $ \session -> do
    res <- Session.run session conn
    case res of
      Left  e        -> liftIO $ throwIO $ ErrorQuery e
      Right (Just e) -> liftIO $ throwIO $ ErrorMigration e
      Right Nothing  -> pure ()
