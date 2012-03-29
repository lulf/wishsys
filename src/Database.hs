{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Application where

import            Control.Monad
import            Control.Monad.State
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as BS
import            Data.Lens.Template
import            Data.Map ((!))
import            Data.Maybe
import            Data.String
import            Database.HDBC.Sqlite3
import            Snap.Core
import            Snap.Snaplet
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Auth.Backends.Hdbc
import            Snap.Snaplet.Hdbc
import            Snap.Snaplet.Session
import            Snap.Snaplet.Session.Backends.CookieSession

data App
   = App
   { _authLens :: Snaplet (AuthManager App)
   , _sessLens :: Snaplet SessionManager
   , _dbLens   :: Snaplet (HdbcSnaplet Connection IO)
   }

makeLens ''App

tutorialAppInit :: SnapletInit App App
tutorialAppInit = makeSnaplet "snaplet-hdbc-tutorial"
  "A tutorial snaplet showing the use of snaplet-hdbc" Nothing $ do
    addRoutes  [ ("/some/:num",  someNumHandler) ]
    _sesslens' <- nestSnaplet "session" sessLens $ initCookieSessionManager
                    "config/site_key.txt" "_session" Nothing
    let sqli = connectSqlite3 "resources/tutorial.db"
    _dblens'   <- nestSnaplet "hdbc" dbLens $ hdbcInit sqli
    _authlens' <- nestSnaplet "auth" authLens $ initHdbcAuthManager
                    defAuthSettings sessLens sqli defAuthTable defQueries
    return  $ App _authlens' _sesslens' _dblens'

data Wish = Wish String Integer Integer deriving Show

 getMessages :: HasHdbc m c s => ByteString -> m [Wish]
 getMessages n = do
   rows <- query "SELECT * FROM messages WHERE somenum = ?" [toSql n]
   return $ map toMsg rows
   where toMsg :: Row -> Message
         toMsg rw = Message $ fromSql (rw ! "msgcol")

