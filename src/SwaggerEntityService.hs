{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SwaggerEntityService
  ( up,
    launchSiteInBrowser,
    swaggerAPI,
    swaggerServer,
  )
where

import           Control.Lens
import           Data.Aeson               (toJSON)
import           Data.Swagger             hiding (port)
import           Models
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI
import           System.Info              (os)
import           System.Process           (createProcess, shell)
import           UserApi                  (UserAPI, userAPI)
import           UserServerSafe           
import           ServerUtils              (mkApp)
import           Database.GP              ( ConnectionPool )

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
swaggerServer :: ConnectionPool -> Server SwaggerAPI
swaggerServer pool =
  swaggerSchemaUIServer
    swaggerDoc
    :<|> userServer pool

-- | start up server and launch browser on swagger UI
up :: IO ()
up = do
  let port = 8080
  app <- mkApp "sqlite.db" swaggerAPI swaggerServer
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
    openUrlWith cmd = createProcess (shell $ cmd ++ " " ++ swaggerUrl)
    swaggerUrl = "http://localhost:" ++ show port ++ "/swagger-ui"
    command = case os of
      "mingw32" -> "start"
      "darwin"  -> "open"
      _         -> "xdg-open"
