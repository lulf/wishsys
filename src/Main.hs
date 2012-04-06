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
import qualified  Data.Text
import            Database.HDBC.Sqlite3
import            Snap
import            Snap.Core
import            Snap.Snaplet
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Auth.Backends.Hdbc
import            Snap.Snaplet.Auth.Backends.JsonFile
import            Snap.Snaplet.Hdbc
import            Snap.Snaplet.Session
import            Snap.Snaplet.Session.Backends.CookieSession
import            Snap.Util.FileServe
import            Data.Maybe
import qualified  Data.ByteString as B

data App = App
   { _authLens :: Snaplet (AuthManager App)
   , _sessLens :: Snaplet SessionManager
   , _dbLens   :: Snaplet (HdbcSnaplet Connection IO)
   }

makeLenses [''App]

appInit :: SnapletInit App App
appInit = makeSnaplet "wishsys" "Wish list application" Nothing $ do
    addRoutes [ ("", serveFile "static/index.html")
              , ("wishlist", handleAsUser "bryllup" wishViewHandler)
              , ("insert", handleAsUser "admin" insertHandler)
              , ("login", with authLens $ loginHandler)
              , ("logout", with authLens $ logoutHandler)
              , ("loginpage", serveFile "static/login.html")
              , ("register", handleAsUser "bryllup" registerHandler)
              , ("admin", handleAsUser "admin" (serveFile "static/admin.html")) ]
              
    _sesslens' <- nestSnaplet "session" sessLens $ initCookieSessionManager "config/site.txt" "_session" Nothing
    _authlens' <- nestSnaplet "auth" authLens $ initJsonFileAuthManager defAuthSettings sessLens "users.json"
    let sqli = connectSqlite3 "config/wishsys.db"
    _dblens'  <- nestSnaplet "hdbc" dbLens $ hdbcInit sqli
    -- Unable to make hdbc work yet
    -- _authlens' <- nestSnaplet "auth" authLens $ initHdbcAuthManager defAuthSettings sessLens sqli defAuthTable defQueries
    return $ App _authlens' _sesslens' _dblens'

main :: IO ()
main = serveSnaplet defaultConfig appInit

--------------------
-- Authentication --
--------------------

-- Verifies user credentials and username before running handler
handleAsUser :: String -> (Handler App App ()) -> Handler App App ()
handleAsUser user fn = do
    mu <- with authLens currentUser
    case mu of
      Just u -> do if (userLogin u) == (Data.Text.pack user)
                      then fn
                      else redirect' "/loginpage" 303
      Nothing -> redirect' "/loginpage" 303

-- Performs the actual login.
loginHandler :: Handler App (AuthManager App) ()
loginHandler = do
    loginUser "login" "password" (Just "remember") onFailure onSuccess
    where onFailure _ = redirect' "/loginpage" 303
          onSuccess = do
                      mu <- currentUser
                      case mu of
                              Just _ -> redirect' "/" 303
                              Nothing -> redirect' "/loginpage" 303 -- Why does this happen?

logoutHandler :: Handler App (AuthManager App) ()
logoutHandler = do
  logout
  redirect' "/" 303

-- Wish data type
data Wish = Wish {
    wishId     :: Integer,
    wishName   :: String,
    wishImg    :: String,
    wishStore  :: String,
    wishAmount :: Integer,
    wishBought :: Integer
}

-- Insert handler deals with inserting new wishes into the database.
insertHandler :: Handler App App ()
insertHandler = do
    what <- getParam "what"
    imgurl <- getParam "imgurl"
    amount <- getParam "amount"
    store <- getParam "store"
    if what == Nothing || imgurl == Nothing || amount == Nothing || store == Nothing
       then writeBS "All three parameters must be set!"
       else do
            let whatText = BS.unpack (fromJust what)
            let urlText = BS.unpack (fromJust imgurl)
            let storeText = BS.unpack (fromJust store)
            let amountValue = read (BS.unpack (fromJust amount)) :: Integer
            insertWish (Wish 0 whatText urlText storeText amountValue 0)
            writeBS (B.concat ["Inserted: '", (fromJust what), "'. Amount: '", (fromJust amount), "'"])

-- Register handler registers an update on a wish
registerHandler :: Handler App App ()
registerHandler = do
    wishid <- getParam "wishid"
    amount <- getParam "amount"
    if wishid == Nothing
       then (writeBS "id not given, aborting")
       else if amount == Nothing
               then (writeBS "amount not specified")
               else do registerPurchase (read (BS.unpack (fromJust wishid)) ::Integer)
                                        (read (BS.unpack (fromJust amount)) ::Integer)

-- Given a wish id and the amount of items, subtract this wish' remaining
-- amount.
registerPurchase :: Integer -> Integer -> Handler App App ()
registerPurchase wishid amount = do
    wish <- getWish wishid
    let wamount = (wishAmount wish)
    let bought = (wishBought wish)
    let remaining = wamount - bought
    if remaining - amount >= 0
       then do
            updateWish wishid (bought + amount)
            writeBS (B.concat ["Har trukket ifra ", (fromString (show amount)), " stk. av type '", (fromString (wishName wish)), "'"])
       else writeBS "Ikke nok ønsker igjen!"

-- Display all wishes and a form for registering purchases
wishViewHandler :: Handler App App ()
wishViewHandler = do
    wishList <- getWishes
    writeBS "<html>"
    writeBS "<h1>Ønskeliste</h1>"
    writeBS "<table border=\"1\">"
    writeBS "<tr><th>Hva</th><th>Bilde</th><th>Butikk</th><th>Antall</th><th>Registrer</th></tr>"
    writeBS (fromString (concat (map formatWishEntry wishList)))
    writeBS "</table>"
    writeBS "</html>"

-- Helper method for formatting a wish entry in the wish view.
formatWishEntry :: Wish -> String
formatWishEntry (Wish wishid name url store amount bought) =
        "<tr>" ++
        "<td>" ++ name ++ "</td>" ++
        "<td><a href=\"" ++ url ++ "\"><img src=\"" ++ url ++ "\" width=\"100\" height=\"100\" /></a></td>" ++ 
        "<td>" ++ store ++ "</td>" ++
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


------------------------------------------
-- Functions for interacting with database
------------------------------------------

-- Get a list of all wishes
getWishes :: HasHdbc m c s => m [Wish]
getWishes = do
    rows <- query "SELECT * FROM list" []
    return $ map constructWish rows

-- Constructs a wish from a database row
constructWish :: Row -> Wish
constructWish row = Wish (fromSql (row ! "id"))
                         (fromSql (row ! "what"))
                         (fromSql (row ! "url"))
                         (fromSql (row ! "store"))
                         (fromSql (row ! "amount"))
                         (fromSql (row ! "bought"))

-- Get a specific wish given an id
getWish :: HasHdbc m c s => Integer -> m Wish
getWish wishid = do
    rows <- query "SELECT * FROM list WHERE id = ?" [toSql wishid]
    return $ head (map constructWish rows)

-- Update a wish (given its id) with a new value for the number of bought items
updateWish :: HasHdbc m c s => Integer -> Integer -> m ()
updateWish wishid bought = do
    query' "UPDATE list SET bought = ? WHERE id = ?" [toSql bought, toSql wishid]
    return ()

-- Insert a new wish entity into the database. The id and bought parameters to
-- the wish are ignored
insertWish:: HasHdbc m c s => Wish -> m ()
insertWish (Wish _ name url store amount _ ) = do
    let sqlList = [toSql name, toSql url, toSql store, toSql amount]
    query' "INSERT INTO list (what, url, store, amount, bought) VALUES(?, ?, ?, ?, 0)" sqlList
    return ()

instance HasHdbc (Handler App App) Connection IO where
    getHdbcState = with dbLens get
