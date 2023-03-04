{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module App where

import           Control.Monad.IO.Class (liftIO)
import           Network.Wai.Handler.Warp ( run )

import           Servant
import           Api
import           Entities
import           Data.Pool
import           Database.GP
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           Database.HDBC (disconnect)

type ConnectionPool = Pool Conn

server :: ConnectionPool -> Server UserAPI
server pool =
  getAllUsersH :<|> getUserH :<|> postUserH :<|> putUserH :<|> deleteUserH
  where
    getAllUsersH = liftIO getAllUsers           -- GET /users
    getUserH idx = liftIO $ getUser idx           -- GET /users/{id}
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

    postUser :: User -> IO ()
    postUser = withConn insert

    putUser :: Id -> User -> IO ()
    putUser _id = withConn update

    deleteUser :: Id -> IO ()
    deleteUser idx = withConn delete user
      where
        user = User idx "name" "email"


app :: ConnectionPool -> Application
app pool = serve userAPI $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- createPool freshConnection disconnect 1 10 10
  return $ app pool
  where
    freshConnection :: IO Conn
    freshConnection = connect SQLite <$> connectSqlite3 sqliteFile


demo :: IO ()
demo = do
  let port = 8080
  putStrLn $ "starting userAPI on port " ++ show port
  a <- mkApp "sqlite.db"
  run port a
