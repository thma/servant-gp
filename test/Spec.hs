{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import           SwaggerEntityService       
import           Test.Hspec
import           Test.Hspec.Wai
import           ServerUtils

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