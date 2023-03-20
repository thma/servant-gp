{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module UserServerSafe
  ( userServer,
    demo,
  )
where

import           Control.Monad.Error.Class      (MonadError)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Data.ByteString.Lazy.Char8     (pack)
import           Database.GP.GenericPersistenceSafe
import           Database.HDBC                  (toSql)
import           Models
import           Network.Wai.Handler.Warp       (run)
import           Servant
import           UserApi                        (UserAPI, userAPI)
import           ConnectionPool                 (ConnectionPool, sqlLitePool, withResource)


userServer :: ConnectionPool -> Server UserAPI
userServer pool =
  getAllUsersH :<|> getUserH :<|> getUserCommentsH :<|> postUserH :<|> putUserH :<|> deleteUserH
  where
    getAllUsersH :: Handler [User]
    getAllUsersH = handleWithConn retrieveAll          -- GET /users
    
    getUserH :: Id -> Handler User
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

    handleWithConn :: (Conn -> IO (Either PersistenceException a)) -> Handler a  -- :: (Conn -> IO a) -> Handler a
    handleWithConn gpAction = do
      eitherExResult <- liftIO $ withResource pool gpAction
      case eitherExResult of
        Left pex     -> throwAsServerError pex
        Right result -> return result

-- | throw a persistence exception as a Servant ServerError
throwAsServerError :: MonadError ServerError m => PersistenceException -> m a
throwAsServerError pex = throwError $ case pex of
  EntityNotFound msg  -> err404 {errBody = format $ "EntityNotFound: " ++ msg}
  DuplicateInsert msg -> err409 {errBody = format $ "DuplicateInsert: " ++ msg}
  DatabaseError msg   -> err500 {errBody = format $ "DatabaseError: " ++ msg}
  NoUniqueKey msg     -> err500 {errBody = format $ "NoUniqueKey: " ++ msg}
  where
    format msg = pack $ "{ \"error\": \"" ++ msg ++ "\" }"

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


