{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           CSDC.API                       ( API
                                                , serveAPI
                                                )
import           CSDC.Config                    ( Context(..)
                                                , activate
                                                , readConfig
                                                , readSecret
                                                , showConfig
                                                )

import qualified CSDC.Auth                     as Auth
import qualified CSDC.DAO                      as DAO
import qualified CSDC.SQL                      as SQL

import           Network.Wai                    ( Middleware )
import           Network.Wai.Handler.Warp       ( defaultSettings
                                                , runSettings
                                                , setLogger
                                                , setPort
                                                )
import           Network.Wai.Logger             ( withStdoutLogger )
import           Servant                        ( Application
                                                , Proxy(..)
                                                , hoistServer
                                                , serve
                                                )
import           System.Environment             ( getArgs )

import qualified Network.Wai.Middleware.Cors   as Cors

args :: IO (FilePath, Maybe FilePath)
args = getArgs >>= \case
  [configPath] -> pure (configPath, Nothing)
  [configPath, secretPath] -> pure (configPath, Just secretPath)
  _ -> error "Usage: csdc-server CONFIG_PATH [SECRET_PATH]"

main :: IO ()
main = do
  (configPath, secretPath) <- args
  readConfig configPath >>= \case
    Nothing     -> error "Could not parse the configuration file."
    Just config -> readSecret secretPath >>= \case
      Nothing     -> error "Could not parse the secrets file."
      Just secret -> do
        putStrLn "Running the server with the following configuration:\n"
        showConfig config
        putStrLn ""
        context <- activate config secret
        putStrLn "Applying migrations..."
        migrate context
        DAO.run (context_dao context) DAO.check
        mainWith context

mainWith :: Context -> IO ()
mainWith context = do
  middleware <- makeMiddleware context
  withStdoutLogger $ \logger -> do
    let port     = context_port context
        path     = context_path context
        settings = setPort port $ setLogger logger defaultSettings
    runSettings settings $ middleware $ application path $ context_dao context

makeMiddleware :: Context -> IO Middleware
makeMiddleware context = do
  authentication <- Auth.middleware (context_auth context)
  let corsOptions = Cors.simpleCorsResourcePolicy
        { Cors.corsRequestHeaders = Cors.simpleHeaders
        }
      cors = Cors.cors (\_ -> Just corsOptions)
  pure $ authentication . cors

application :: FilePath -> DAO.Context -> Application
application path context request response =
  let proxy  = Proxy @API
      server = hoistServer proxy (DAO.run context) (serveAPI path)
  in  do
        serve proxy server request response

migrate :: Context -> IO ()
migrate context = do
  let path = context_migration context
  DAO.run (context_dao context) $ DAO.runSQL $ SQL.migrate path
