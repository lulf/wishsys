{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import            Control.Monad
import            Control.Monad.State
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as BS
import            Data.Lens.Template
import            Data.Map ((!))
import            Data.Maybe
import            Data.String
-- import            Database.HDBC.Sqlite3
import            Snap
import            Snap.Core
import            Snap.Snaplet
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Auth.Backends.Hdbc
import            Snap.Snaplet.Hdbc
import            Snap.Snaplet.Session
import            Snap.Snaplet.Session.Backends.CookieSession
import            Snap.Util.FileServe
import            Data.Maybe
import qualified  Data.ByteString as B

data App = App
--   { _authLens :: Snaplet (AuthManager App)
--   , _sessLens :: Snaplet SessionManager
--   , _dbLens   :: Snaplet (HdbcSnaplet Connection IO)
--   }

makeLenses [''App]

appInit :: SnapletInit App App
appInit = makeSnaplet "wishsys" "Wish list application" Nothing $ do
    addRoutes [ ("", serveFile "static/index.html")
              , ("insert", insertHandler)
              , ("admin", serveFile "static/admin.html") ]
    return $ App

main :: IO ()
main = serveSnaplet defaultConfig appInit

insertHandler :: Handler b v ()
insertHandler = do
    what <- getParam "what"
    amount <- getParam "amount"
    if what == Nothing
       then (writeBS "must specify 'what'")
       else if amount == Nothing
               then (writeBS "must specify 'amount'")
               else (writeBS (B.concat ["What: '", (fromJust what), "', Amount: '", (fromJust amount), "'"]))
