{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}

module EntityService 
  ( UserAPI, 
    userAPI, 
    userServer,
    app,
    demo,
  )

where

import           Control.Exception          hiding (Handler)
import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy.Char8 (pack)
import           Database.GP
import           Database.HDBC.Sqlite3
import           Description                (Desc)
import           Entities
import           Network.HTTP.Types.Status  (status404, status409, status500)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Exception          (ToServantErr, status)

-- | REST api for User Entities
type UserAPI = --Throws PersistenceException :>
       "users" :> Summary "retrieve all users"
               -- :> QueryParam' '[Optional, Desc Int "max number of records to load"] "maxRecords" Int
               :> Get '[ JSON] [User]
  :<|> "users" :> Summary "retrieve user identified by :id"
               :> Capture' '[Desc Id "unique identifier"] ":id" Id
               :> Get  '[ JSON] User
  :<|> "users" :> Summary "store a new user"
               :> ReqBody '[ JSON] User
               :> Post '[ JSON] ()
  :<|> "users" :> Summary "update existing user"
               :> Capture' '[Desc Id "unique identifier"] ":id" Id
               :> ReqBody '[ JSON] User
               :> Put '[ JSON] ()
  :<|> "users" :> Summary "delete existing user"
               :> Capture' '[Desc Id "unique identifier"] ":id" Id
               :> Delete '[ JSON] ()
               
-- | implements the UserAPI
userServer :: Server UserAPI
userServer =
  getAllUsers -- GET /users
    :<|> getUser -- GET /users/{id}
    :<|> postUser -- POST /users
    :<|> putUser -- POST /users/{id}
    :<|> deleteUser -- DELETE /users/{id}

dbUrl :: String
dbUrl = "sqlite.db"

freshConnection :: IO Conn
freshConnection = connect SQLite <$> connectSqlite3 dbUrl

-- | handler functions
getAllUsers :: Handler [User]
getAllUsers = do
  liftIO $ putStrLn "GET /users"
  eitherUsersEx <- liftIO $ try getAll :: Handler (Either PersistenceException [User])
  case eitherUsersEx of
    Left ex -> throwAsServerError ex
    Right l -> return l

getAll :: IO [User]
getAll = do
  conn <- freshConnection
  retrieveAll conn

getUser :: Id -> Handler User
getUser idx = do
  liftIO $ putStrLn $ "GET /users/" ++ show idx
  eitherUserEx <- liftIO $ try (get idx) :: Handler (Either PersistenceException User)
  case eitherUserEx of
    Left ex -> throwAsServerError ex
    Right u -> return u

get :: Id -> IO User
get idx = do
  conn <- freshConnection
  maybeUser <- retrieveById conn idx
  case maybeUser of
    Nothing -> throwIO $ EntityNotFound $ "User with id " ++ show idx ++ " not found"
    Just u -> return u

postUser :: User -> Handler ()
postUser user = do
  liftIO $ putStrLn $ "POST /users/ " ++ show user
  eitherVoidEx <- liftIO $ try (liftIO $ post user) :: Handler (Either PersistenceException ())
  case eitherVoidEx of
    Left ex -> throwAsServerError ex
    Right v -> return v

post :: User -> IO ()
post user = do
  conn <- freshConnection
  insert conn user

putUser :: Id -> User -> Handler ()
putUser idx user = do
  liftIO $ putStrLn $ "PUT /users/" ++ show idx ++ " " ++ show user
  eitherVoidEx <- liftIO $ try (put user) :: Handler (Either PersistenceException ())
  case eitherVoidEx of
    Left ex -> throwAsServerError ex
    Right v -> return v

put :: User -> IO ()
put user = do
  conn <- freshConnection
  update conn user

deleteUser :: Id -> Handler ()
deleteUser idx = do
  liftIO $ putStrLn $ "DELETE /users/" ++ show idx
  eitherVoidEx <- liftIO $ try (del idx) :: Handler (Either PersistenceException ())
  case eitherVoidEx of
    Left ex -> throwAsServerError ex
    Right v -> return v

del :: Id -> IO ()
del idx = do
  conn <- freshConnection
  delete conn user
  where
    user = User idx "name" "email"

-- | throw a persistence exception as a Servant ServerError
throwAsServerError :: MonadError ServerError m => PersistenceException -> m a
throwAsServerError ex =
  throwError $
    case ex of
      EntityNotFound msg      -> err404 {errBody = pack msg}
      EntityAlreadyExists msg -> err409 {errBody = pack msg}
      InternalError msg       -> err500 {errBody = pack msg}

data PersistenceException
  = EntityNotFound String
  | EntityAlreadyExists String
  | InternalError String
  deriving (Show, Exception)

instance ToServantErr PersistenceException where
  status (EntityNotFound _msg)      = status404
  status (EntityAlreadyExists _msg) = status409
  status (InternalError _msg)       = status500

-- | boilerplate to guide type inference
userAPI :: Proxy UserAPI
userAPI = Proxy

-- the following is not actually needed, it just exists for local testing
-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve userAPI userServer

demo :: IO ()
demo = do
  let port = 8080
  putStrLn $ "starting userAPI on port " ++ show port
  run port app
