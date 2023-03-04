{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Api 
  (
  UserAPI,
  userAPI,  
  )
where
  
import Data.Proxy
import Servant.API
import Description
import Entities

-- | REST api for User Entities
type UserAPI = --Throws PersistenceException :>
       "users" :> Summary "retrieve all users"
               -- :> QueryParam' '[Optional, Desc Int "max number of records to load"] "maxRecords" Int
               :> Get '[ JSON] [User]
  :<|> "users" :> Summary "retrieve user identified by :id"
               :> Capture' '[Desc Id "unique identifier"] ":id" Id
               :> Get  '[ JSON] (Maybe User)
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
               -- | boilerplate to guide type inference

userAPI :: Proxy UserAPI
userAPI = Proxy