{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import           EntityService       (app)
import           Test.Hspec
import           Test.Hspec.Wai

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  with (return app) $ do describe "GET /users" $ do { it "responds with 200" $ do { get "/users" `shouldRespondWith` 200 } }
