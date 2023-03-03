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
import           Data.Maybe

type ConnectionPool = Pool Conn

server :: ConnectionPool -> Server UserAPI
server pool =
  getAllUsersH :<|> getUserH :<|> postUserH :<|> putUserH :<|> deleteUserH
  where
    getAllUsersH = liftIO getAllUsers           -- GET /users
    getUserH id = liftIO $ getUser id           -- GET /users/{id}
    postUserH user = liftIO $ postUser user     -- POST /users
    putUserH id user = liftIO $ putUser id user -- PUT /users/{id}
    deleteUserH id = liftIO $ deleteUser id     -- DELETE /users/{id}

    withPooledConn :: (Conn -> IO a) -> IO a
    withPooledConn = withResource pool

    getAllUsers :: IO [User]
    getAllUsers = withPooledConn retrieveAll

    getUser :: Id -> IO User
    getUser idx = fromJust <$> withPooledConn (`retrieveById` idx)

    postUser :: User -> IO ()
    postUser user = withPooledConn (`insert` user)

    putUser :: Id -> User -> IO ()
    putUser _id user = withPooledConn (`update` user)

    deleteUser :: Id -> IO ()
    deleteUser id = withPooledConn (`delete` user)
          where
            user = User id "name" "email"


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
