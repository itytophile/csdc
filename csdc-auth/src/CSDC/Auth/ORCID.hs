{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

--------------------------------------------------------------------------------
-- References
--
-- https://members.orcid.org/api/about-public-api
-- https://members.orcid.org/api/integrate/orcid-sign-in
-- https://members.orcid.org/api/oauth/3legged-oauth
-- https://members.orcid.org/api/tutorial/read-orcid-records
--
--------------------------------------------------------------------------------

module CSDC.Auth.ORCID
  ( -- * Configuration
    Config(..)
  , Endpoint(..)
  ,

    -- * OAuth2
    oauth2
  ,

    -- * User Identity
    Token(..)
  , Id(..)
  , Scope(..)
  ,

    -- * User Record
    getUserRecord
  ) where

import           CSDC.Auth.OAuth2               ( OAuth2(..) )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (.=)
                                                , FromJSON(..)
                                                , ToJSON(..)
                                                , Value(..)
                                                , object
                                                , withObject
                                                , withText
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified Network.HTTP.Client.TLS       as HTTP.TLS
import           Network.OAuth.OAuth2           ( AccessToken
                                                , RefreshToken
                                                , authGetJSON
                                                )
import           Network.Wai.Middleware.Auth.Provider
                                                ( ProviderInfo(..) )
import           URI.ByteString                 ( parseURI
                                                , strictURIParserOptions
                                                )

--------------------------------------------------------------------------------
-- Configuration

data Config = Config
  { config_id       :: Text
  , config_secret   :: Text
  , config_endpoint :: Endpoint
  }
  deriving (Show, Eq)

instance FromJSON Config where
  parseJSON = withObject "Config"
    $ \o -> Config <$> (o .: "id") <*> (o .: "secret") <*> (o .: "endpoint")

instance ToJSON Config where
  toJSON (Config uid secret endpoint) =
    object ["id" .= uid, "secret" .= secret, "endpoint" .= endpoint]

data Endpoint = Production | Sandbox
  deriving (Show, Eq)

instance FromJSON Endpoint where
  parseJSON = withText "Endpoint" $ \case
    "production" -> pure Production
    "sandbox"    -> pure Sandbox
    _            -> fail "Endpoint must be 'production' or 'sandbox'."

instance ToJSON Endpoint where
  toJSON Production = String "production"
  toJSON Sandbox    = String "sandbox"

makeHost :: Endpoint -> Text
makeHost Production = "https://orcid.org"
makeHost Sandbox    = "https://sandbox.orcid.org"

makeAPIHost :: Endpoint -> Text
makeAPIHost Production = "https://pub.orcid.org"
makeAPIHost Sandbox    = "https://pub.sandbox.orcid.org"

--------------------------------------------------------------------------------
-- OAuth

oauth2 :: Config -> OAuth2 Token
oauth2 config = OAuth2
  { oa2ClientId            = config_id config
  , oa2ClientSecret        = config_secret config
  , oa2AuthorizeEndpoint   = makeHost (config_endpoint config)
                               <> "/oauth/authorize"
  , oa2AccessTokenEndpoint = makeHost (config_endpoint config) <> "/oauth/token"
  , oa2Scope               = Just ["/authenticate"]
  , oa2ProviderInfo        = ProviderInfo
    { providerTitle   = "ORCID"
    , providerLogoUrl =
      "https://orcid.org/sites/default/files/images/orcid_16x16.png"
    , providerDescr   = "Login using your ORCID account."
    }
  }

--------------------------------------------------------------------------------
-- User Identity

data Token = Token
  { token_access  :: AccessToken
  , token_type    :: Maybe Text
  , token_refresh :: Maybe RefreshToken
  , token_expires :: Maybe Int
  , token_scope   :: Scope
  , token_orcid   :: Id
  , token_name    :: Text
  }
  deriving Show

newtype Scope = Scope Text
  deriving newtype (Show, Eq, FromJSON, ToJSON)

newtype Id = Id {getId :: Text}
  deriving newtype (Show, Eq, FromJSON, ToJSON)

instance FromJSON Token where
  parseJSON = withObject "Token" $ \o ->
    Token
      <$> (o .: "access_token")
      <*> (o .:? "token_type")
      <*> (o .:? "refresh_token")
      <*> (o .:? "expires_in")
      <*> (o .: "scope")
      <*> (o .: "orcid")
      <*> (o .: "name")

instance ToJSON Token where
  toJSON token = object
    [ "access_token" .= token_access token
    , "token_type" .= token_type token
    , "refresh_token" .= token_refresh token
    , "expires_in" .= token_expires token
    , "scope" .= token_scope token
    , "orcid" .= token_orcid token
    , "name" .= token_name token
    ]

--------------------------------------------------------------------------------
-- Get Record

getUserRecord :: Endpoint -> AccessToken -> Id -> IO (Either ByteString Value)
getUserRecord endpoint token (Id user) = do
  let url = makeAPIHost endpoint <> "/v3.0/" <> user <> "/record"
      uri = case parseURI strictURIParserOptions (encodeUtf8 url) of
        Left  e -> error (show e)
        Right a -> a

  manager <- HTTP.TLS.newTlsManager
  authGetJSON manager token uri
