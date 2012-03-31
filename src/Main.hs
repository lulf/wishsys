{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import            Control.Monad
import            Control.Monad.State
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as BS
import            Data.Lens.Template
import            Data.Map ((!))
import            Data.Maybe
import            Data.String
import            Database.HDBC.Sqlite3
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
   { --_authLens :: Snaplet (AuthManager App)
--   , _sessLens :: Snaplet SessionManager
     _dbLens   :: Snaplet (HdbcSnaplet Connection IO)
   }

makeLenses [''App]

appInit :: SnapletInit App App
appInit = makeSnaplet "wishsys" "Wish list application" Nothing $ do
    addRoutes [ ("", serveFile "static/index.html")
              , ("wishlist", wishViewHandler)
              , ("insert", insertHandler)
              , ("admin", serveFile "static/admin.html") ]
    let sqli = connectSqlite3 "test/test.db"
    _dblens'  <- nestSnaplet "hdbc" dbLens $ hdbcInit sqli
    return $ App _dblens'

main :: IO ()
main = serveSnaplet defaultConfig appInit

insertHandler :: Handler App App ()
insertHandler = do
    what <- getParam "what"
    amount <- getParam "amount"
    if what == Nothing
       then (writeBS "must specify 'what'")
       else if amount == Nothing
               then (writeBS "must specify 'amount'")
               else (writeBS (B.concat ["What: '", (fromJust what), "', Amount: '", (fromJust amount), "'"]))

formatWishEntry :: Wish -> String
formatWishEntry (Wish wishid name amount bought) =
        "<tr>" ++
        "<td>" ++ name ++ "</td>" ++
        "<td>" ++ (show amount) ++ "</td>" ++
        "<td>" ++ (show remaining) ++ "</td>" ++
        "<td>" ++
        "<form action=\"register\" method=\"post\">" ++
        "<input type=\"text\" size=\"2\" name=\"amount\" value=\"0\" />" ++
        "<input type=\"hidden\" name=\"wishid\" value=\"" ++ (show wishid) ++ "\" />" ++
        "<input type=\"submit\" value=\"Registrer\" />" ++
        "</form>" ++
        "</td>" ++
        "</tr>"
    where remaining = amount - bought

wishViewHandler :: Handler App App ()
wishViewHandler = do
    wishList <- getWishes
    writeBS "<html>"
    writeBS "<h1>Ønskeliste</h1>"
    writeBS "<table border=\"1\">"
    writeBS "<tr><th>Hva</th><th>Antall</th><th>Gjenstående</th><th>Registrer</th></tr>"
    writeBS (fromString (concat (map formatWishEntry wishList)))
    writeBS "</table>"
    writeBS "</html>"

data Wish = Wish {
    wishId     :: Integer,
    wishName   :: String,
    wishAmount :: Integer,
    wishBought :: Integer
}

getWishes :: HasHdbc m c s => m [Wish]
getWishes = do
    rows <- query "SELECT * FROM list" []
    return $ map toWish rows
    where toWish :: Row -> Wish
          toWish rw = Wish (fromSql (rw ! "id")) (fromSql (rw ! "what")) (fromSql (rw ! "amount")) (fromSql (rw ! "bought"))


instance HasHdbc (Handler App App) Connection IO where
    getHdbcState = with dbLens get
