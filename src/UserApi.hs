{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}


module UserApi 
  (
  UserAPI,
  userAPI,  
  )
where
  
import Data.Proxy
import Servant.API
import Description
import Models

-- | REST api for User Entities
type UserAPI = --Throws PersistenceException :>
       "users" :> Summary "retrieve all users"
               -- :> QueryParam' '[Optional, Describe Int "max number of records to load"] "maxRecords" Int
               :> Get '[ JSON] [User]
  :<|> "users" :> Summary "retrieve user identified by :id"
               :> Capture' '[Describe Id "unique User identifier"] ":id" Id
               :> Get  '[ JSON] User
  :<|> "users" :> Summary "retrieve comments identified by user :id"
               :> Capture' '[Describe Id "unique User identifier"] ":id" Id
               :> "comments" 
               :> Get  '[ JSON] [Comment]
  :<|> "users" :> Summary "store a new user"
               :> ReqBody '[ JSON] User
               :> Post '[ JSON] ()
  :<|> "users" :> Summary "update existing user"
               :> Capture' '[Describe Id "unique User identifier"] ":id" Id
               :> ReqBody '[ JSON] User
               :> Put '[ JSON] ()
  :<|> "users" :> Summary "delete existing user"
               :> Capture' '[Describe Id "unique User identifier"] ":id" Id
               :> Delete '[ JSON] ()
               
-- | boilerplate to guide type inference
userAPI :: Proxy UserAPI
userAPI = Proxy


