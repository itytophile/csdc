{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CSDC.Auth.Admin
  ( Token(..)
  , tokenParser
  ) where

import           CSDC.Auth.User                 ( User(..) )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , encode
                                                , withText
                                                )
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text.Encoding             ( decodeUtf8With
                                                , encodeUtf8
                                                )
import           Data.Text.Encoding.Error       ( lenientDecode )
import           Network.HTTP.Types             ( status403 )
import           Network.Wai                    ( queryString )
import           Network.Wai.Middleware.Auth.Provider
                                                ( AuthProvider(..)
                                                , ProviderInfo(..)
                                                , ProviderParser
                                                , mkProviderParser
                                                )

--------------------------------------------------------------------------------
-- Token

newtype Token user
  = Token {token_value :: ByteString}
  deriving (Show, Eq)

instance ToJSON (Token user) where
  toJSON (Token bs) = toJSON $ decodeUtf8With lenientDecode bs

instance FromJSON (Token user) where
  parseJSON = withText "Token" $ \txt -> pure $ Token $ encodeUtf8 txt

tokenParser :: forall user . ToJSON user => Proxy user -> ProviderParser
tokenParser _ = mkProviderParser (Proxy :: Proxy (Token user))

instance ToJSON user => AuthProvider (Token user) where
  getProviderName _ = "admin"

  getProviderInfo _ = ProviderInfo
    { providerTitle   = "Admin Token"
    , providerLogoUrl = "http://lol"
    , providerDescr   = "Authentication using an admin token."
    }

  handleLogin (Token token) request _ _ onSuccess onFailure = do
    let params = queryString request
    case lookup "token" params of
      Just (Just token') -> if token == token'
        then onSuccess $ toStrict $ encode (Admin :: User user)
        else onFailure status403 "Token incorrect, access denied."
      _ -> onFailure status403 "Token not found, access denied."
