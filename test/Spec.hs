{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import           ServerUtils          (mkApp, setUpSchema)
import           SwaggerEntityService (swaggerAPI, swaggerServer)
import           Test.Hspec           (Spec, describe, hspec, it)
import           Test.Hspec.Wai       (get, shouldRespondWith, with)

main :: IO ()
main = hspec spec

db :: FilePath
db = "sqlite.db"

spec :: Spec
spec = do
  with prepareApp $
    describe "GET /users" $
      it "responds with 200" $
        get "/users" `shouldRespondWith` 200
  where
    prepareApp = do
      setUpSchema db
      mkApp db swaggerAPI swaggerServer
