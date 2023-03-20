module ConnectionPool 
  ( ConnectionPool, 
    sqlLitePool, 
    withResource,
  )
where

import           Data.Pool                      (Pool, createPool, withResource)
import           Database.GP.GenericPersistence (Conn (..), connect, Database (..))
import Database.HDBC
import Database.HDBC.Sqlite3

type ConnectionPool = Pool Conn

sqlLitePool :: FilePath -> IO ConnectionPool
sqlLitePool sqliteFile = createPool freshConnection disconnect 1 10 10
  where
    freshConnection :: IO Conn
    freshConnection = connect SQLite <$> connectSqlite3 sqliteFile