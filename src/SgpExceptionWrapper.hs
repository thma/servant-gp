{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module SgpExceptionWrapper     
  ( --connect,
    delete,
    insert,
    insertMany,
    retrieveAll,
    retrieveAllWhere,
    retrieveById,
    setupTableFor,
    update,
    --Conn,
    --Database(SQLite) 
    PersistenceException(..),
  ) where

import qualified Database.GP as GP

import Control.Exception ( try, Exception, SomeException, throw )
import Database.HDBC (SqlValue)
import Data.List (isInfixOf)

delete :: GP.Entity a => GP.Conn -> a -> IO ()
delete c e = do 
  eitherExOk <- try $ GP.delete c e
  case eitherExOk of
    Left ex -> throw $ fromException ex
    Right _ -> return ()
  
insert :: GP.Entity a => GP.Conn -> a -> IO ()
insert c e = do 
  eitherExOk <- try $ GP.insert c e
  case eitherExOk of
    Left ex -> throw $ handleEx ex
    Right _ -> return ()
  where
    handleEx :: SomeException -> PersistenceException
    handleEx ex = if "UNIQUE constraint failed" `isInfixOf` show ex 
      then EntityAlreadyExists "Entity already exists in DB, use update instead" 
      else fromException ex

insertMany :: GP.Entity a => GP.Conn -> [a] -> IO ()
insertMany c es = do 
  eitherExOk <- try $ GP.insertMany c es
  case eitherExOk of
    Left ex -> throw $ fromException ex
    Right _ -> return ()

retrieveAll :: GP.Entity a => GP.Conn -> IO [a]
retrieveAll c = do 
  eitherExOk <- try $ GP.retrieveAll c
  case eitherExOk of
    Left ex -> throw $ fromException ex
    Right es -> return es

retrieveAllWhere :: GP.Entity a => GP.Conn -> String -> SqlValue -> IO [a]
retrieveAllWhere c col val = do 
  eitherExOk <- try $ GP.retrieveAllWhere c col val
  case eitherExOk of
    Left ex -> throw $ fromException ex
    Right es -> return es

retrieveById :: forall a . GP.Entity a => GP.Conn -> Int -> IO a
retrieveById c idx = do 
  eitherExOk <- try $ GP.retrieveById c idx
  case eitherExOk of
    Left ex -> throw $ fromException ex
    Right maybeE -> case maybeE of
      Nothing -> throw $ EntityNotFound $ GP.constructorName (GP.typeInfo  @a) ++ " " ++ show idx ++ " not found"
      Just e -> return e

setupTableFor :: forall a . (GP.Entity a) => GP.Conn -> IO ()
setupTableFor c = do 
  eitherExOk <- try $ GP.setupTableFor @a c
  case eitherExOk of
    Left ex -> throw $ fromException ex
    Right _ -> return ()

update :: GP.Entity a => GP.Conn -> a -> IO ()
update c e = do 
  eitherExOk <- try $ GP.update c e
  case eitherExOk of
    Left ex -> throw $ fromException ex
    Right _ -> return ()

fromException :: SomeException -> PersistenceException
fromException = InternalError . show

  -- | exceptions that may occur during persistence operations
data PersistenceException = 
    EntityNotFound String
  | EntityAlreadyExists String
  | InternalError String
  deriving (Show, Exception)