{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Entities
  ( Id,
    User (..),
    BlogPost (..),
    Comment (..),
    setUpSchema,
  )
where

import           Data.Aeson
import           Database.GP
import           Database.HDBC.Sqlite3
import           GHC.Generics

type Id = Int

data User = User
  { userID :: Id,
    name   :: String,
    email  :: String
  }
  deriving (Show, Read, Generic, Entity, ToJSON, FromJSON)

data BlogPost = BlogPost
  { postID  :: Id,
    userRef :: Id,
    text    :: String
  }
  deriving (Show, Read, Generic, Entity, ToJSON, FromJSON)

data Comment = Comment
  { commentID :: Id,
    userRef   :: Id,
    postRef   :: Id,
    text      :: String
  }
  deriving (Show, Read, Generic, Entity, ToJSON, FromJSON)

setUpSchema :: IO ()
setUpSchema = do
  conn <- connect SQLite <$> connectSqlite3 "sqlite.db"
  setupTableFor @User conn
  setupTableFor @BlogPost conn
  setupTableFor @Comment conn
