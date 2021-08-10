{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified CSDC.Auth                     as Auth
import           Data.Aeson                     ( decodeFileStrict
                                                , encode
                                                )
import           Network.HTTP.Types             ( status200 )
import           Network.Wai                    ( responseLBS )
import qualified Network.Wai.Handler.Warp      as Warp

--------------------------------------------------------------------------------
--A server that tests the request methods. Both require cookies. For admin
--requests:
--
--  curl --cookie-jar cookies \
--    http://localhost:3000/_auth_middleware/admin?token=SUPER_SECRET_TOKEN
--
main :: IO ()
main = decodeFileStrict "auth-config.json" >>= \case
  Nothing     -> error "Could not decode orcid-config.json file."
  Just config -> do
    let application request respond = do
          let token = Auth.getUserToken request
          print token
          respond $ responseLBS status200 [] $ encode token

    putStrLn "Starting."
    middleware <- Auth.middleware config
    Warp.run 3000 $ middleware application
