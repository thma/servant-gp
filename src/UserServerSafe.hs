{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module UserServerSafe
  ( userServer,
    demoSafe,
  )
where

import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Database.GP.GenericPersistenceSafe
import           Models
import           Network.Wai.Handler.Warp       (run)
import           Servant
import           UserApi                        (UserAPI, userAPI)
import           ServerUtils

userServer :: ConnectionPool -> Server UserAPI
userServer pool =
  getAllUsersH :<|> getUserH :<|> getUserCommentsH :<|> postUserH :<|> putUserH :<|> deleteUserH
  where
    getAllUsersH :: Handler [User]
    getAllUsersH = handleWithConn $ \conn ->           -- GET /users
      select conn allEntries
    
    getUserH :: Id -> Handler User
    getUserH idx = handleWithConn (`selectById` idx)   -- GET /users/{id}

    getUserCommentsH :: Id -> Handler [Comment]
    getUserCommentsH idx = handleWithConn $ \conn ->
      select conn (field "userRef" =. idx)             -- GET /users/{id}/comments

    postUserH :: User -> Handler ()
    postUserH user = handleWithConn (`insert` user)    -- POST /users
    
    putUserH :: Id -> User -> Handler ()
    putUserH _id user = handleWithConn (`update` user) -- PUT /users/{id}
    
    deleteUserH :: Id -> Handler ()
    deleteUserH idx = handleWithConn (`delete` user)   -- DELETE /users/{id}
      where
        user = User idx "name" "email"

    handleWithConn :: (Conn -> IO (Either PersistenceException a)) -> Handler a  -- :: (Conn -> IO a) -> Handler a
    handleWithConn gpAction = do
      eitherExResult <- liftIO $ withResource pool gpAction
      case eitherExResult of
        Left pex     -> throwAsServerError pex
        Right result -> return result


demoSafe :: IO ()
demoSafe = do
  let port = 8080
  putStrLn $ "starting userAPI on port " ++ show port
  a <- mkApp "sqlite.db" userAPI userServer
  run port a


