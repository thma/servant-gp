{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module UserServer
  ( userServer,
    demo,
  )
where

import           Control.Exception        (throw, try)
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Database.GP              hiding (run)
import           Models
import           Network.Wai.Handler.Warp (run)
import           Servant
import           ServerUtils
import           UserApi                  (UserAPI, userAPI)

userServer :: ConnectionPool -> Server UserAPI
userServer pool =
  getAllUsersH :<|> getUserH :<|> getUserCommentsH :<|> postUserH :<|> putUserH :<|> deleteUserH
  where
    getAllUsersH :: Handler [User]
    getAllUsersH = handleWithConn $ \conn ->
      select conn allEntries -- GET /users
    getUserH :: Id -> Handler User
    getUserH idx =
      handleWithConn $
        nothingToPex (`selectById` idx) -- GET /users/{id}
      where
        nothingToPex :: (Conn -> IO (Maybe a)) -> Conn -> IO a
        nothingToPex gpAction conn = do
          maybeUser <- gpAction conn
          case maybeUser of
            Nothing -> throw $ EntityNotFound "User not found"
            Just u  -> return u
    getUserCommentsH :: Id -> Handler [Comment]
    getUserCommentsH idx = handleWithConn $ \conn ->
      select conn (field "userRef" =. idx) -- GET /users/{id}/comments
    postUserH :: User -> Handler User
    postUserH user = handleWithConn (`insert` user) -- POST /users
    putUserH :: Id -> User -> Handler ()
    putUserH _id user = handleWithConn (`update` user) -- PUT /users/{id}
    deleteUserH :: Id -> Handler ()
    deleteUserH idx = handleWithConn (`delete` user) -- DELETE /users/{id}
      where
        user = User idx "name" "email"

    handleWithConn :: (Conn -> IO a) -> Handler a
    handleWithConn gpAction = do
      eitherExResult <- liftIO $ try $ withResource pool gpAction
      case eitherExResult of
        Left pex     -> throwAsServerError pex
        Right result -> return result

demo :: IO ()
demo = do
  let port = 8080
  putStrLn $ "starting userAPI on port " ++ show port
  a <- mkApp "sqlite.db" userAPI userServer
  run port a
