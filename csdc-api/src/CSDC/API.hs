{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CSDC.API
  ( API,
    serveAPI,

    -- * Utils
    Auth,
  )
where

import qualified CSDC.API.DAO as DAO
import CSDC.Auth (getUserToken)
import CSDC.Prelude ( UserToken, HasDAO )
import CSDC.User (runUserT)
import Servant
    ( Proxy(Proxy),
      hoistServer,
      serveDirectoryWith,
      type (:<|>)(..),
      Raw,
      type (:>),
      HasServer(..) )
import Servant.Server.Internal.Delayed (passToServer)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (StaticSettings (..), unsafeToPiece)

--------------------------------------------------------------------------------
-- API

type API =
  Auth :> "api" :> DAO.API
    :<|> Raw

serveAPI :: HasDAO m => FilePath -> ServerT API m
serveAPI path =
  serveDAOAPI
    :<|> serveDirectoryWith (options path)
  where
    serveDAOAPI token =
      hoistServer (Proxy @DAO.API) (runUserT token) DAO.serveAPI

options :: FilePath -> StaticSettings
options path =
  let base = defaultWebAppSettings path

      indexRedirect old = \case
        [] -> old [unsafeToPiece "index.html"]
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
