{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

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
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Auth.Backends.JsonFile
import            Snap.Snaplet.Hdbc
import            Snap.Snaplet.Session
import            Snap.Snaplet.Session.Backends.CookieSession
import            Snap.Util.FileServe

-- User configurable

guestUsers :: [String]
guestUsers = ["bryllup"]
adminUsers :: [String]
adminUsers = ["admin"]

-- Application setup

data App = App
   { _authLens :: Snaplet (AuthManager App)
   , _sessLens :: Snaplet SessionManager
   , _dbLens   :: Snaplet (HdbcSnaplet Connection IO)
   }

makeLenses [''App]

appInit :: SnapletInit App App
appInit = makeSnaplet "wishsys" "Wish list application" Nothing $ do
    addAuthRoutes [ ("wishlist", wishViewHandler, guestUsers)
                  , ("admin", adminHandler, adminUsers) ]
    addRoutes [ ("", serveFile "static/index.html")
              , ("public/stylesheets", serveDirectory "public/stylesheets")
              , ("login/:ref", loginHandler)
              , ("login", loginHandler)
              , ("logout", logoutHandler) ]
              
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

-- Add routes that are authenticated by a user
addAuthRoutes :: [(ByteString, Handler App App (), [String])] -> Initializer App App () 
addAuthRoutes routeList = do
    let authRouteList = map createAuthRoute routeList
    addRoutes authRouteList
    
-- FIXME: Support more than one user
createAuthRoute :: (ByteString, Handler App App (), [String]) -> (ByteString, Handler App App ())
createAuthRoute (routePath, handler, (user:_)) = (routePath, handleAsUser user handler)
createAuthRoute (routePath, handler, []) = (routePath, handler)

redirectLogin :: MonadSnap m => m a
redirectLogin = do
    req <- getRequest
    let uri = rqURI req
    redirect $ BS.concat ["/login", uri]

-- Creates the login page
createLoginPage :: String -> String
createLoginPage referrer = "<html>" ++
                           "<body>" ++
                           "<h1>Du må logge inn for å få tilgang til denne siden</h1>" ++
                           "<form action=\"/login/" ++ referrer ++ "\" method=\"post\">" ++
                           "<p>Brukernavn:</p><input type=\"text\" size=\"20\" name=\"login\" value=\"\" /><br />" ++
                           "<p>Passord:</p><input type=\"password\" size=\"20\" name=\"password\" value=\"\" /><br />" ++
                           "<input type=\"hidden\" name=\"referrer\" value=\"/" ++ referrer ++ "\" /><br />" ++
                           "<input type=\"submit\" value=\"login\" />" ++
                           "</form>" ++
                           "</body>" ++
                           "</html>"

-- Displays the login page, and preserve the referrer header
loginForm :: Handler App (AuthManager b) ()
loginForm = do
    ref <- getParam "ref"
    case ref of
             Nothing -> writeBS (BS.pack (createLoginPage ""))
             Just val  -> writeBS (BS.pack (createLoginPage (BS.unpack val)))


-- Performs the actual login.
loginHandler :: Handler App App ()
loginHandler = with authLens $ do
    loginUser "login" "password" (Just "remember") onFailure onSuccess
    where onFailure _ = do loginForm
          onSuccess   = do
                        mu <- currentUser
                        case mu of
                                Just _ -> do ref <- getParam "referrer"
                                             redirectTo ref
                                Nothing -> do loginForm -- Why does this happen?

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
              Just uri -> redirect uri

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

pageHeader :: String -> String
pageHeader header =
    "<html>" ++
    "<head>" ++
    "<title>Ønskesys</title>" ++
    "<link rel=\"stylesheet\" href=\"public/stylesheets/style.css\" type=\"text/css\" />" ++
    "</head>" ++
    "<body>" ++
    "<h1>" ++ header ++ "</h1>"

pageFooter :: String
pageFooter =
    "<a href=\"/logout\">Logg ut</a>" ++
    "</body>" ++
    "</html>"

render :: MonadSnap m => String -> m ()
render text = writeBS (BS.pack text)

adminHandler :: Handler App App ()
adminHandler = do
    render $ pageHeader "Administrer ønskeliste"
    insertHandler
    printWishList True
    render insertForm
    render pageFooter

insertForm :: String
insertForm =
    "<h3>Sett inn nytt ønske</h3>" ++
    "<form action=\"/admin\" method=\"post\">" ++
    "<input type=\"text\" size=\"200\" name=\"what\" value=\"Skriv inn ønske\" /><br />" ++
    "<input type=\"text\" size=\"200\" name=\"imgurl\" value=\"URL til bilde\" /><br />" ++
    "<input type=\"text\" size=\"200\" name=\"store\" value=\"Navn på butikk + evt. url\" /><br />" ++
    "<input type=\"text\" size=\"2\" name=\"amount\" value=\"0\" /><br />" ++
    "<input type=\"submit\" value=\"Registrer\" />" ++
    "</form>"

-- Insert handler deals with inserting new wishes into the database.
insertHandler :: Handler App App () --MonadSnap m => m b -> Maybe ByteString -- Handler App App ()
insertHandler = do
    whatParam <- getParam "what"
    imgurlParam <- getParam "imgurl"
    amountParam <- getParam "amount"
    storeParam <- getParam "store"
    case (whatParam, imgurlParam, amountParam, storeParam) of
         (Just what,
          Just imgurl,
          Just amount,
          Just store) ->  do
                          let whatText = BS.unpack what
                          let urlText = BS.unpack imgurl
                          let storeText = BS.unpack store
                          let amountValue = read (BS.unpack amount) :: Integer
                          insertWish (Wish 0 whatText urlText storeText amountValue 0)
                          render $ "La inn " ++ (show amountValue) ++ " stk. av '" ++ whatText ++ "'"
         _            ->  return ()


-- Handler for the wishlist view. Registers any purchases and displays wish
-- list.
wishViewHandler :: Handler App App ()
wishViewHandler = do
    wishidParam <- getParam "wishid"
    amountParam <- getParam "amount"
    render $ pageHeader "Registrere kjøpt ønske"
    case (wishidParam, amountParam) of
         (Just wishid, Just amount) -> do registerPurchase (read (BS.unpack wishid) ::Integer)
                                                           (read (BS.unpack amount) ::Integer)
                                          printWishList False
         _                          -> do printWishList False
    render pageFooter

-- Given a wish id and the amount of items, subtract this wish' remaining
-- amount.
registerPurchase :: Integer -> Integer -> Handler App App ()
registerPurchase wishid amount = do
    wish <- getWish wishid
    let bought = (wishBought wish)
    updateWish wishid (bought + amount)
    render $ concat ["<p>Registrerte ", show amount, " stk. av '", wishName wish, "'</p>"]

-- Display all wishes and a form for registering purchases
printWishList :: Bool -> Handler App App ()
printWishList admin = do
    wishList <- getWishes
    render $ formatWishList wishList admin

formatWishList :: [Wish] -> Bool -> String
formatWishList wishList admin =
        "<h3>Ønskeliste</h3>" ++
        "<table border=\"1\">" ++
        "<tr><th>Hva</th><th>Bilde</th><th>Butikk</th>" ++
        userHeaders ++
        "</tr>" ++
        wishes ++
        "</table>"
    where wishes      = concat (map (\x -> formatWishEntry x admin) wishList)
          userHeaders = if admin then "" else "<th>Antall</th><th>Registrer</th>"

-- Helper method for formatting a wish entry in the wish view.
formatWishEntry :: Wish -> Bool -> String
formatWishEntry (Wish wishid name url store amount bought) admin =
        "<tr>" ++
        "<td>" ++ name ++ "</td>" ++
        "<td><a href=\"" ++ url ++ "\"><img src=\"" ++ url ++ "\" width=\"100\" height=\"100\" /></a></td>" ++ 
        "<td>" ++ store ++ "</td>" ++
        userHeaders ++
        "</tr>"
    where remaining    = amount - bought
          remainingCol = if remaining <= 0
                            then "<p color=\"#00ff00\">" ++ (show remaining) ++ "</p>"
                            else (show remaining)
          userHeaders  = if admin
                           then ""
                           else "<td>" ++ remainingCol ++ "</td>" ++
                                "<td>" ++
                                "<form action=\"/wishlist\" method=\"post\">" ++
                                "<input type=\"text\" size=\"2\" name=\"amount\" value=\"0\" />" ++
                                "<input type=\"hidden\" name=\"wishid\" value=\"" ++ (show wishid) ++ "\" />" ++
                                "<input type=\"submit\" value=\"Registrer\" />" ++
                                "</form>" ++
                                "</td>"

---------------------------------------------
-- Functions for interacting with database --
---------------------------------------------

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
