{-# LANGUAGE OverloadedStrings #-}

module CSDC.Auth
  ( -- * Authentication middleware
    Config(..)
  , middleware
  ,

    -- * Authentication tokens
    UserToken
  , getUserToken
  ) where

import qualified CSDC.Auth.Admin               as Admin
import qualified CSDC.Auth.ORCID               as ORCID
import           CSDC.Auth.User                 ( User(..) )
import           Data.Aeson                     ( (.:)
                                                , (.=)
                                                , FromJSON(..)
                                                , ToJSON(..)
                                                , eitherDecode
                                                , object
                                                , withObject
                                                )
import qualified Data.ByteString.Lazy          as L
import qualified Data.HashMap.Strict           as HashMap
import           GHC.Stack                      ( HasCallStack )
import           Network.Wai                    ( Middleware
                                                , Request
                                                )
import           Network.Wai.Middleware.Auth    ( AuthSettings
                                                , defaultAuthSettings
                                                , mkAuthMiddleware
                                                , setAuthProviders
                                                , setAuthSessionAge
                                                )
import qualified Network.Wai.Middleware.Auth   as MA
import           Network.Wai.Middleware.Auth.Provider
                                                ( Provider(..)
                                                , authLoginState
                                                )

data Config = Config
  { config_orcid :: ORCID.Config
  , config_admin :: Admin.Token ORCID.Token
  }
  deriving (Show, Eq)

instance ToJSON Config where
  toJSON config =
    object ["orcid" .= config_orcid config, "admin" .= config_admin config]

instance FromJSON Config where
  parseJSON =
    withObject "Config" $ \o -> Config <$> o .: "orcid" <*> o .: "admin"

--------------------------------------------------------------------------------
-- Middleware

settings :: Config -> AuthSettings
settings config =
  let providers = HashMap.fromList
        [ ("orcid", Provider $ ORCID.oauth2 $ config_orcid config)
        , ("admin", Provider $ config_admin config)
        ]
  in  setAuthProviders providers
        $ setAuthSessionAge 360000
        $ defaultAuthSettings

middleware :: Config -> IO Middleware
middleware = mkAuthMiddleware . settings

type UserToken = User ORCID.Token

getUserToken :: HasCallStack => Request -> UserToken
getUserToken request = case getAccessToken request of
  Nothing    -> error "No user identity after authorization middleware."
  Just token -> token

getAccessToken :: FromJSON a => Request -> Maybe a
getAccessToken req = do
  user <- MA.getAuthUser req
  either (const Nothing) Just $ eitherDecode $ L.fromStrict $ authLoginState
    user
