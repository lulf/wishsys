{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Config where

-- Third party.
import            Control.Monad.State
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as BS (concat, pack, unpack)
import            Data.Lens.Template
import            Data.Map ((!))
import qualified  Data.Text
import            Database.HDBC.Sqlite3
import            Snap
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Heist as H
import            Snap.Snaplet.Auth.Backends.JsonFile
import            Snap.Snaplet.Hdbc
import            Snap.Snaplet.Session
import            Snap.Snaplet.Session.Backends.CookieSession
import            Snap.Util.FileServe
import qualified  Text.Blaze.Html5 as HTML
import qualified  Text.Blaze.Html5.Attributes as ATTR
import            Text.Blaze.Renderer.XmlHtml

-- User configurable

guestUsers :: [String]
guestUsers = ["bryllup"]
adminUsers :: [String]
adminUsers = ["admin"]

-- Application setup

data App = App
   { _heist     :: Snaplet (Heist App)
   , _authLens  :: Snaplet (AuthManager App)
   , _sessLens  :: Snaplet SessionManager
   , _dbLens    :: Snaplet (HdbcSnaplet Connection IO)
   }

makeLenses [''App]

instance HasHeist App where heistLens = subSnaplet heist
