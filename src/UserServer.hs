{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module UserServer 
  ( userServer,
    demo,
    ConnectionPool,
    sqlLitePool,
    setUpSchema
  ) 
where

import           Control.Monad.IO.Class (liftIO)
import           Network.Wai.Handler.Warp ( run )

import           Servant
import           UserApi
import           Models
import           Data.Pool
import           Database.GP
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           Database.HDBC (disconnect, toSql)

type ConnectionPool = Pool Conn

userServer :: ConnectionPool -> Server UserAPI
userServer pool =
  getAllUsersH :<|> getUserH :<|> getUserCommentsH :<|> postUserH :<|> putUserH :<|> deleteUserH
  where
    getAllUsersH = liftIO getAllUsers           -- GET /users
    getUserH idx = liftIO $ getUser idx           -- GET /users/{id}
    getUserCommentsH idx = liftIO $ getUserComments idx -- GET /users/{id}/comments
    postUserH user = liftIO $ postUser user     -- POST /users
    putUserH idx user = liftIO $ putUser idx user -- PUT /users/{id}
    deleteUserH idx = liftIO $ deleteUser idx     -- DELETE /users/{id}

    withPooledConn :: (Conn -> IO a) -> IO a
    withPooledConn = withResource pool

    withConn :: (Conn -> b -> IO a) -> b -> IO a
    withConn f b = withResource pool (`f` b)

    getAllUsers :: IO [User]
    getAllUsers = withPooledConn retrieveAll

    getUser :: Id -> IO (Maybe User)
    getUser = withConn retrieveById

    getUserComments :: Id -> IO [Comment]
    getUserComments idx = withPooledConn $ \conn ->
      retrieveAllWhere conn "userRef" (toSql idx)  -- select * from comments where userRef = idx

    postUser :: User -> IO ()
    postUser = withConn insert

    putUser :: Id -> User -> IO ()
    putUser _id = withConn update

    deleteUser :: Id -> IO ()
    deleteUser idx = withConn delete user
      where
        user = User idx "name" "email"


sqlLitePool :: FilePath -> IO ConnectionPool
sqlLitePool sqliteFile = createPool freshConnection disconnect 1 10 10
  where
    freshConnection :: IO Conn
    freshConnection = connect SQLite <$> connectSqlite3 sqliteFile

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- sqlLitePool sqliteFile
  return $ serve userAPI $ userServer pool



demo :: IO ()
demo = do
  let port = 8080
  putStrLn $ "starting userAPI on port " ++ show port
  a <- mkApp "sqlite.db"
  run port a

setUpSchema :: IO ()
setUpSchema = do
  conn <- connect SQLite <$> connectSqlite3 "sqlite.db"
  setupTableFor @User conn
  setupTableFor @BlogPost conn
  setupTableFor @Comment conn

  let users = [User 1 "Alice" "alice@mail.com", User 2 "Bob" "bob@mail.com"]
  let posts = [BlogPost 1 1 "A text by Alice", BlogPost 2 2 "Bobs first post"]
  let comments = [Comment 1 2 1 "Alice' comment", Comment 2 1 2 "Bob's comment"]

  insertMany conn users
  insertMany conn posts
  insertMany conn comments
