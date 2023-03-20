{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ExplicitNamespaces #-}
module ServerUtils 
  (
    setUpSchema,
    mkApp
  )
where
import Models
import Database.GP
import Database.HDBC.Sqlite3
import ConnectionPool
import Servant
import Data.Proxy

setUpSchema :: FilePath -> IO ()
setUpSchema db = do
  conn <- connect SQLite <$> connectSqlite3 db
  setupTableFor @User conn
  setupTableFor @BlogPost conn
  setupTableFor @Comment conn

  let users = [User 1 "Alice" "alice@mail.com", User 2 "Bob" "bob@mail.com"]
  let posts = [BlogPost 1 1 "A text by Alice", BlogPost 2 2 "Bobs first post"]
  let comments = [Comment 1 2 1 "Alice' comment", Comment 2 1 2 "Bob's comment"]

  _ <- insertMany conn users
  _ <- insertMany conn posts
  _ <- insertMany conn comments
  pure ()

-- | create an application from a db file name
--mkApp :: HasServer api '[] => FilePath -> Proxy api -> (ConnectionPool -> ServerT api Handler) -> IO Application
mkApp sqliteFile api serverFun = do
  pool <- sqlLitePool sqliteFile
  return $ serve api (serverFun pool)