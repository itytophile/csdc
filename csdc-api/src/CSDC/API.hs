{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module CSDC.API
  ( API,
    serveAPI,

    -- * Utils
    Auth,
  )
where

import qualified CSDC.API.DAO                    as DAO
import           CSDC.Auth                       (getUserToken)
import           CSDC.Prelude                    (HasDAO, UserToken)
import           CSDC.User                       (runUserT)
import           Control.Lens
import           Data.Swagger
import           Servant
import           Servant.Server.Internal.Delayed (passToServer)
import           Servant.Swagger
import           WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import           WaiAppStatic.Types              (StaticSettings (..),
                                                  unsafeToPiece)

--------------------------------------------------------------------------------
-- API
type CsdcApi = "api" :> DAO.API
csdcAPI :: Proxy CsdcApi
csdcAPI = Proxy

type API =
  Auth :> CsdcApi
    :<|> "swagger.json" :> Get '[JSON] Swagger
    :<|> Raw

csdcSwagger :: Swagger
csdcSwagger = toSwagger csdcAPI
  & info.title   .~ "CSDC DAO API"
  & info.version .~ "0.1"
  & info.description ?~ "The API used by the Elm app"

serveAPI :: HasDAO m => FilePath -> ServerT API m
serveAPI path =
  serveDAOAPI
    :<|> return csdcSwagger
    :<|> serveDirectoryWith (CSDC.API.options path)
  where
    serveDAOAPI token =
      hoistServer (Proxy @DAO.API) (runUserT token) DAO.serveAPI

options :: FilePath -> StaticSettings
options path =
  let base = defaultWebAppSettings path

      indexRedirect old = \case
        []  -> old [unsafeToPiece "index.html"]
        pcs -> old pcs
   in base {ssLookupFile = indexRedirect (ssLookupFile base)}

--------------------------------------------------------------------------------
-- Auth

-- | This type is used for representing authentication credentials at the
-- servant API level.
data Auth

instance HasServer api context => HasServer (Auth :> api) context where
  type ServerT (Auth :> api) m = UserToken -> ServerT api m

  route Proxy context subserver =
    route (Proxy @api) context (passToServer subserver getUserToken)

  hoistServerWithContext _ pc nat s =
    hoistServerWithContext (Proxy @api) pc nat . s
