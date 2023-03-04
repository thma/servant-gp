{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models
  ( Id,
    User (..),
    BlogPost (..),
    Comment (..),
  )
where

import           Data.Aeson ( FromJSON, ToJSON )
import           Database.GP (Entity)
import           GHC.Generics ( Generic )

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

