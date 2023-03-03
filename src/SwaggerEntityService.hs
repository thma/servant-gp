{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SwaggerEntityService 
  (
    up,
    app,
    launchSiteInBrowser,
  )
where

import           Control.Lens
import           Data.Aeson               (toJSON)
import           Data.Swagger             hiding (port)
import           Entities                 (User (..))
import           EntityService            (UserAPI, userAPI, userServer)
import           Network.Wai
import           Network.Wai.Handler.Warp ( run )
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI
import           System.Info              (os)
import           System.Process           (createProcess, shell)

-- | Swagger spec of Model type 'User'
instance ToSchema User where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "This is a User API (tm)"
      & mapped . schema . example ?~ toJSON (User 4711 "Max Muster" "mm@muster.com")

-- | Swagger spec for user API.
swaggerDoc :: Swagger
swaggerDoc =
  toSwagger userAPI
    & info . title .~ "User API"
    & info . version .~ "1.23"
    & info . description ?~ "This is an API that tests swagger integration"
    & info . license ?~ ("APACHE 2.0" & url ?~ URL "http://apache.org")

-- | API type with bells and whistles, i.e. schema file and swagger-ui.
type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> UserAPI

-- | boilerplate to guide type inference
api :: Proxy API
api = Proxy

-- | Servant server for an API
server :: Server API
server =
  swaggerSchemaUIServer
    swaggerDoc
    :<|> userServer

-- | 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve api server

-- | start up server and launch browser on swagger UI
up :: IO ()
up = do
  let port = 8080
  putStrLn $ "GET all users: http://localhost:" ++ show port ++ "/users"
  putStrLn $ "GET user 1:    http://localhost:" ++ show port ++ "/users/1"
  putStrLn $ "Swagger UI:    http://localhost:" ++ show port ++ "/swagger-ui"
  launchSiteInBrowser port
  run port app

-- | convenience function that opens the swagger UI in the default web browser
launchSiteInBrowser :: Int -> IO ()
launchSiteInBrowser port = do
  _ <- openUrlWith command
  return ()
  where
    swaggerUrl = "http://localhost:" ++ show port ++ "/swagger-ui"
    openUrlWith cmd = createProcess (shell $ cmd ++ " " ++ swaggerUrl)
    command = case os of
      "mingw32" -> "start"
      "darwin"  -> "open"
      _         -> "xdg-open"
