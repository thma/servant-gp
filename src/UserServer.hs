{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module UserServer
  ( userServer,
    demo,
    ConnectionPool,
    sqlLitePool,
    setUpSchema,
  )
where

import           Control.Exception              (try)
import           Control.Monad.Error.Class      (MonadError)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Data.ByteString.Lazy.Char8     (pack)
import           Data.Pool                      (Pool, createPool, withResource)
import           Database.GP.GenericPersistence
import           Database.HDBC                  (disconnect, toSql)
import           Database.HDBC.Sqlite3          (connectSqlite3)
import           Models
import           Network.Wai.Handler.Warp       (run)
import           Servant
import           UserApi                        (UserAPI, userAPI)

type ConnectionPool = Pool Conn

userServer :: ConnectionPool -> Server UserAPI
userServer pool =
  getAllUsersH :<|> getUserH :<|> getUserCommentsH :<|> postUserH :<|> putUserH :<|> deleteUserH
  where
    getAllUsersH :: Handler [User]
    getAllUsersH = handleWithConn retrieveAll          -- GET /users
    
    getUserH :: Id -> Handler (Maybe User)
    getUserH idx = handleWithConn (`retrieveById` idx) -- GET /users/{id}
    
    getUserCommentsH :: Id -> Handler [Comment]
    getUserCommentsH idx = handleWithConn $ \conn ->
      retrieveAllWhere conn "userRef" (toSql idx)      -- GET /users/{id}/comments

    postUserH :: User -> Handler ()
    postUserH user = handleWithConn (`insert` user)    -- POST /users
    
    putUserH :: Id -> User -> Handler ()
    putUserH _id user = handleWithConn (`update` user) -- PUT /users/{id}
    
    deleteUserH :: Id -> Handler ()
    deleteUserH idx = handleWithConn (`delete` user)   -- DELETE /users/{id}
      where
        user = User idx "name" "email"

    handleWithConn :: (Conn -> IO a) -> Handler a
    handleWithConn gpAction = do
      eitherExResult <- liftIO $ try $ withResource pool gpAction
      case eitherExResult of
        Left pex     -> throwAsServerError pex
        Right result -> return result

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
