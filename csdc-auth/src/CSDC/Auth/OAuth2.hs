-- This module is inlined from wai-middleware-auth, with some changes so we can
-- decode the ORCID token.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module CSDC.Auth.OAuth2
  ( OAuth2(..)
  , URIParseException(..)
  ) where

import           CSDC.Auth.User                 ( User(..) )

import           Control.Monad.Catch            ( Exception
                                                , MonadThrow(..)
                                                )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , eitherDecode
                                                , encode
                                                )
import           Data.Aeson.TH                  ( defaultOptions
                                                , deriveJSON
                                                , fieldLabelModifier
                                                )
import           Data.Text.Encoding             ( decodeUtf8With
                                                , encodeUtf8
                                                )
import           Data.Text.Encoding.Error       ( lenientDecode )
import           Network.HTTP.Client.Conduit    ( Manager )
import           Network.HTTP.Client.TLS        ( getGlobalManager )
import           Network.HTTP.Conduit           ( Manager )
import           Network.HTTP.Types             ( status303
                                                , status403
                                                , status404
                                                , status501
                                                )
import           Network.Wai                    ( queryString
                                                , responseLBS
                                                )
import           Network.Wai.Middleware.Auth.Provider
                                                ( AuthProvider
                                                  ( getProviderInfo
                                                  , getProviderName
                                                  , handleLogin
                                                  )
                                                , ProviderInfo
                                                , ProviderUrl(ProviderUrl)
                                                )
import           URI.ByteString                 ( URI )

import qualified Data.ByteString               as S
import qualified Data.ByteString.Char8         as S8
                                                ( pack )
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.Text                     as T
import qualified Network.OAuth.OAuth2          as OA2
import qualified Network.OAuth.OAuth2.TokenRequest
                                               as TR
import qualified URI.ByteString                as U

-- | General OAuth2 authentication `Provider`.
data OAuth2 user = OAuth2
  { oa2ClientId            :: T.Text
  , oa2ClientSecret        :: T.Text
  , oa2AuthorizeEndpoint   :: T.Text
  , oa2AccessTokenEndpoint :: T.Text
  , oa2Scope               :: Maybe [T.Text]
  , oa2ProviderInfo        :: ProviderInfo
  }

fetchAccessTokenBSL
  :: Manager                                   -- ^ HTTP connection manager
  -> OA2.OAuth2                                     -- ^ OAuth Data
  -> OA2.ExchangeToken                              -- ^ OAuth2 Code
  -> IO (OA2.OAuth2Result TR.Errors BSL.ByteString) -- ^ Access Token
fetchAccessTokenBSL manager oa code = OA2.doSimplePostRequest manager
                                                              oa
                                                              uri
                                                              body
  where (uri, body) = OA2.accessTokenUrl oa code

{- | Used for validating proper url structure. Can be thrown by
 `parseAbsoluteURI` and consequently by `handleLogin` for `OAuth2` `Provider`
 instance.

 @since 0.1.2.0
-}
newtype URIParseException
  = URIParseException U.URIParseError
  deriving Show

instance Exception URIParseException

{- | Parse absolute URI and throw `URIParseException` in case it is malformed

 @since 0.1.2.0
-}
parseAbsoluteURI :: MonadThrow m => T.Text -> m U.URI
parseAbsoluteURI urlTxt = do
  case U.parseURI U.strictURIParserOptions (encodeUtf8 urlTxt) of
    Left  err -> throwM $ URIParseException err
    Right url -> return url

parseAbsoluteURI' :: MonadThrow m => T.Text -> m U.URI
parseAbsoluteURI' = parseAbsoluteURI

getExchangeToken :: S.ByteString -> OA2.ExchangeToken
getExchangeToken = OA2.ExchangeToken . decodeUtf8With lenientDecode

appendQueryParams :: URI -> [(S.ByteString, S.ByteString)] -> URI
appendQueryParams uri params = OA2.appendQueryParams params uri

getClientId :: T.Text -> T.Text
getClientId = id

getClientSecret :: T.Text -> T.Text
getClientSecret = id

getRedirectURI :: U.URIRef a -> S.ByteString
getRedirectURI = U.serializeURIRef'

instance (FromJSON user, ToJSON user) => AuthProvider (OAuth2 user) where
  getProviderName _ = "orcid"
  getProviderInfo = oa2ProviderInfo
  handleLogin oa2@OAuth2 {..} req suffix renderUrl onSuccess onFailure = do
    authEndpointURI <- parseAbsoluteURI' oa2AuthorizeEndpoint
    accessTokenEndpointURI <- parseAbsoluteURI' oa2AccessTokenEndpoint
    callbackURI <- parseAbsoluteURI' $ renderUrl (ProviderUrl ["complete"]) []
    let oauth2 = OA2.OAuth2
          { oauthClientId            = getClientId oa2ClientId
          , oauthClientSecret        = Just $ getClientSecret oa2ClientSecret
          , oauthOAuthorizeEndpoint  = authEndpointURI
          , oauthAccessTokenEndpoint = accessTokenEndpointURI
          , oauthCallback            = Just callbackURI
          }
    case suffix of
      [] -> do
        let scope = encodeUtf8 . T.intercalate "," <$> oa2Scope
        let redirectUrl = getRedirectURI $ appendQueryParams
              (OA2.authorizationUrl oauth2)
              (maybe [] ((: []) . ("scope", )) scope)
        return $ responseLBS status303
                             [("Location", redirectUrl)]
                             "Redirect to OAuth2 Authentication server"
      ["complete"] ->
        let params = queryString req
        in
          case lookup "code" params of
            Just (Just code) -> do
              man  <- getGlobalManager
              eRes <- fetchAccessTokenBSL man oauth2 $ getExchangeToken code
              case eRes of
                Left  err   -> onFailure status501 $ S8.pack $ show err
                Right token -> case eitherDecode token of
                  Left _ -> onFailure status501 "Could not decode token."
                  Right (user :: user) ->
                    onSuccess $ L.toStrict $ encode $ User user
            _ -> case lookup "error" params of
              (Just (Just "access_denied")) ->
                onFailure status403 "User rejected access to the application."
              (Just (Just error_code)) ->
                onFailure status501 $ "Received an error: " <> error_code
              (Just Nothing) ->
                onFailure status501
                  $  "Unknown error connecting to "
                  <> encodeUtf8 (getProviderName oa2)
              Nothing -> onFailure
                status404
                "Page not found. Please continue with login."
      _ -> onFailure status404 "Page not found. Please continue with login."

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''OAuth2)
