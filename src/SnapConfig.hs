{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module SnapConfig where

-- Third party.
import           Control.Monad.State
import           Control.Lens
import           Database.HDBC.Sqlite3
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Heist    as H
import           Snap.Snaplet.Session

-- Application setup
data App = App
   { _heist     :: Snaplet (Heist App)
   , _authLens  :: Snaplet (AuthManager App)
   , _sessLens  :: Snaplet SessionManager
   , _dbLens    :: Snaplet (HdbcSnaplet Connection IO)
   }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist
instance HasHdbc (Handler App App) Connection IO where
    getHdbcState = with dbLens get
