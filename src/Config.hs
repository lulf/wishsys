{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Config where

-- Third party.
import            Control.Monad.State
import            Data.Lens.Template
import            Database.HDBC.Sqlite3
import            Snap
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Heist as H
import            Snap.Snaplet.Hdbc
import            Snap.Snaplet.Session

-- User configurable

guestUsers :: [String]
guestUsers = ["bryllup"]
adminUsers :: [String]
adminUsers = ["admin"]

wishDB :: String
wishDB = "config/wishsys.db"

userDB :: String
userDB = "users.json"

siteKey :: String
siteKey = "config/site.txt"

-- Application setup

data App = App
   { _heist     :: Snaplet (Heist App)
   , _authLens  :: Snaplet (AuthManager App)
   , _sessLens  :: Snaplet SessionManager
   , _dbLens    :: Snaplet (HdbcSnaplet Connection IO)
   }

makeLenses [''App]

instance HasHeist App where heistLens = subSnaplet heist
instance HasHdbc (Handler App App) Connection IO where
    getHdbcState = with dbLens get
