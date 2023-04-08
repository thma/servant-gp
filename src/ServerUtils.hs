{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ExistentialQuantification, DataKinds, PolyKinds, GADTs #-}
module ServerUtils
  (
    setUpSchema,
    mkApp,
    throwAsServerError,
  )
where
import Models
import Database.GP
import Database.HDBC.Sqlite3
import Servant
import           Data.ByteString.Lazy.Char8     (pack)
import           Control.Monad.Error.Class      (MonadError)

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

-- | throw a persistence exception as a Servant ServerError
throwAsServerError :: MonadError ServerError m => PersistenceException -> m a
throwAsServerError pex = throwError $ case pex of
  EntityNotFound msg  -> err404 {errBody = format $ "EntityNotFound: " ++ msg}
  DuplicateInsert msg -> err409 {errBody = format $ "DuplicateInsert: " ++ msg}
  DatabaseError msg   -> err500 {errBody = format $ "DatabaseError: " ++ msg}
  NoUniqueKey msg     -> err500 {errBody = format $ "NoUniqueKey: " ++ msg}
  where
    format msg = pack $ "{ \"error\": \"" ++ msg ++ "\" }"

-- | create an application from a db file name and a server
mkApp sqliteFile api serverFun = do
  pool <- sqlLitePool sqliteFile
  return $ serve api (serverFun pool)

sqlLitePool :: FilePath -> IO ConnectionPool
sqlLitePool sqlLiteFile = createConnPool SQLite sqlLiteFile connectSqlite3 10 100