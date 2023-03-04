{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import           SwaggerEntityService       (app)
import           Test.Hspec
import           Test.Hspec.Wai
import           UserServer (sqlLitePool, setUpSchema, ConnectionPool)


main :: IO ()
main = hspec spec

prepareDB :: IO ConnectionPool
prepareDB = do
  setUpSchema
  sqlLitePool "sqlite.db"

spec :: Spec
spec = do
  with (app <$> prepareDB) $
    describe "GET /users" $
      it "responds with 200" $
        get "/users" `shouldRespondWith` 200
