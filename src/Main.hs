{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

-- import            Control.Monad
import            Control.Monad.State
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as BS (concat, pack, unpack)
import            Data.Lens.Template
import            Data.Map ((!))
import            Data.Maybe
import            Data.String
import qualified  Data.Text
import            Database.HDBC.Sqlite3
import            Snap
-- import            Snap.Core
-- import            Snap.Snaplet
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Auth.Backends.JsonFile
import            Snap.Snaplet.Hdbc
import            Snap.Snaplet.Session
import            Snap.Snaplet.Session.Backends.CookieSession
import            Snap.Util.FileServe

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
              , ("login", loginHandler)
              , ("logout", logoutHandler)
              , ("loginpage/:ref", loginPageHandler)
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

redirectLogin :: MonadSnap m => m a
redirectLogin = do
    req <- getRequest
    let uri = rqURI req
    redirect $ BS.concat ["/loginpage", uri]

-- Creates the login page
createLoginPage :: String -> String
createLoginPage referrer = "<html>" ++
                           "<body>" ++
                           "<h1>Du må logge inn for å få tilgang til denne siden</h1>" ++
                           "<form action=\"/login\" method=\"post\">" ++
                           "<input type=\"text\" size=\"10\" name=\"login\" value=\"\" />" ++
                           "<input type=\"password\" size=\"20\" name=\"password\" value=\"\" />" ++
                           "<input type=\"hidden\" name=\"referrer\" value=\"" ++ referrer ++ "\" />" ++
                           "<input type=\"submit\" value=\"login\" />" ++
                           "</form>" ++
                           "</body>" ++
                           "</html>"

-- Displays the login page, and preserve the referrer header
loginPageHandler :: Handler App App ()
loginPageHandler = do
    ref <- getParam "ref"
    case ref of
             Nothing -> writeBS (BS.pack (createLoginPage ""))
             Just val  -> writeBS (BS.pack (createLoginPage (BS.unpack val)))

-- Verifies user credentials and username before running handler
handleAsUser :: String -> (Handler App App ()) -> Handler App App ()
handleAsUser user fn = do
    mu <- with authLens currentUser
    case mu of
      Just u -> do if (userLogin u) == (Data.Text.pack user)
                      then fn
                      else redirectLogin
      Nothing -> redirectLogin

-- Redirect to a value if set
redirectTo :: MonadSnap m => Maybe ByteString -> m b
redirectTo dest = do
    case dest of
              Nothing -> redirect "/"
              Just _ -> redirect (fromJust dest)

-- Performs the actual login.
loginHandler :: Handler App App ()
loginHandler = with authLens $ do
    loginUser "login" "password" (Just "remember") onFailure onSuccess
    where onFailure _ = do redirectLogin
          onSuccess = do
                      mu <- currentUser
                      case mu of
                              Just _ -> do ref <- getParam "referrer"
                                           redirectTo ref
                              Nothing -> do redirectLogin -- Why does this happen?

logoutHandler :: Handler App App ()
logoutHandler = do
  with authLens logout
  redirect "/"

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
            writeBS (BS.concat ["Inserted: '", (fromJust what), "'. Amount: '", (fromJust amount), "'"])
            redirect "/admin"

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
                       redirect "/wishlist"

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
            writeBS (BS.concat ["Har trukket ifra ", (fromString (show amount)), " stk. av type '", (fromString (wishName wish)), "'"])
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
    writeBS "<a href=\"/logout\">Logg ut</a>"
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
