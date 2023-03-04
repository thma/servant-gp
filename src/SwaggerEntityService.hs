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
import           Models                
import UserApi ( UserAPI, userAPI )
import UserServer ( ConnectionPool, userServer, sqlLitePool )
import           Network.Wai.Handler.Warp ( run )
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI
import           System.Info              (os)
import           System.Process           (createProcess, shell)
import Data.Text (unpack)

-- | Swagger spec of Model type 'User'
instance ToSchema User where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "This is the schema for model type User"
      & mapped . schema . example ?~ toJSON (User 4711 "Max Muster" "mm@muster.com")

instance ToSchema Comment where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "This is the schema for model type Comment"
      & mapped . schema . example ?~ toJSON (Comment 1 1 2 "A comment for a blog post.")

-- | Swagger spec for user API.
swaggerDoc :: Swagger
swaggerDoc =
  toSwagger userAPI
    & info . title .~ "User API"
    & info . version .~ "1.23"
    & info . description ?~ "This is an API for users, their blogs and their comments."
    & info . license ?~ ("APACHE 2.0" & url ?~ URL "http://apache.org")

-- | API type with bells and whistles, i.e. schema file and swagger-ui.
type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> UserAPI

-- | boilerplate to guide type inference
swaggerAPI :: Proxy SwaggerAPI
swaggerAPI = Proxy

-- | Servant server for an API
server :: ConnectionPool -> Server SwaggerAPI
server pool =
  swaggerSchemaUIServer
    swaggerDoc
    :<|> userServer pool

-- | 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: ConnectionPool -> Application
app pool = serve swaggerAPI (server pool)

-- | start up server and launch browser on swagger UI
up :: IO ()
up = do
  let port = 8080
  pool <- sqlLitePool "sqlite.db"
  putStrLn $ "GET all users: http://localhost:" ++ show port ++ "/users"
  putStrLn $ "GET user 1:    http://localhost:" ++ show port ++ "/users/1"
  putStrLn $ "Swagger UI:    http://localhost:" ++ show port ++ "/swagger-ui"
  putStrLn $ unpack $ layout userAPI
  launchSiteInBrowser port
  run port (app pool)

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
