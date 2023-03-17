{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE LambdaCase #-}

module UserServer 
  ( userServer,
    demo,
    ConnectionPool,
    sqlLitePool,
    setUpSchema
  ) 
where

import           Control.Monad.IO.Class ( MonadIO(liftIO) )
import           Network.Wai.Handler.Warp ( run )
import           Control.Monad.Error.Class  (MonadError)
import           Servant
import           UserApi ( userAPI, UserAPI )
import           Models
import           Data.Pool ( createPool, withResource, Pool )
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           Database.HDBC (disconnect, toSql)
import           Control.Exception ( try )
import           Data.ByteString.Lazy.Char8 (pack)
import           Database.GP.GenericPersistence

type ConnectionPool = Pool Conn

userServer :: ConnectionPool -> Server UserAPI
userServer pool =
  getAllUsersH :<|> getUserH :<|> getUserCommentsH :<|> postUserH :<|> putUserH :<|> deleteUserH
  where
    getAllUsersH :: Handler [User]
    getAllUsersH = handleWithConn retrieveAll           -- GET /users

    getUserH :: Id -> Handler (Maybe User)
    getUserH idx = handleWithConn (`retrieveById` idx)  -- GET /users/{id}

    getUserCommentsH :: Id -> Handler [Comment]
    getUserCommentsH idx = handleWithConn $ \conn ->    -- GET /users/{id}/comments
      retrieveAllWhere conn "userRef" (toSql idx)       

    postUserH :: User -> Handler ()
    postUserH user = handleWithConn (`insert` user)     -- POST /users

    putUserH :: Id -> User -> Handler ()
    putUserH _idx user = handleWithConn (`update` user) -- PUT /users/{id}

    deleteUserH :: Id -> Handler ()
    deleteUserH idx = handleWithConn (`delete` user)    -- DELETE /users/{id}
      where
        user = User idx "name" "email"

    handleWithConn :: (Conn -> IO a) -> Handler a
    handleWithConn gpAction = do
      eitherExEntity <- liftIO $ try $ withResource pool gpAction
      case eitherExEntity of
        Left pex -> throwAsServerError pex
        Right entity -> return entity


-- | throw a persistence exception as a Servant ServerError
throwAsServerError :: MonadError ServerError m => PersistenceException -> m a
throwAsServerError pex = throwError $ case pex of
  EntityNotFound msg  -> err404 {errBody = format msg}
  DuplicateInsert msg -> err409 {errBody = format msg}
  DatabaseError msg   -> err500 {errBody = format msg}
  NoUniqueKey msg     -> err500 {errBody = format msg}
  where 
    format msg = pack $ "{ \"error\": \"" ++ msg ++ "\" }"

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

  pure ()
