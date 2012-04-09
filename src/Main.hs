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

appInit :: SnapletInit App App
appInit = makeSnaplet "wishsys" "Wish list application" Nothing $ do
    addAuthRoutes [ ("wishlist", wishViewHandler, guestUsers)
                  , ("admin", adminHandler, adminUsers) ]
    addRoutes [ ("", mainHandler)
              , ("test", heistServe)
              , ("public/stylesheets", serveDirectory "public/stylesheets")
              , ("login/:ref", loginHandler)
              , ("login", loginHandler)
              , ("logout", logoutHandler) ]
              
    _heistlens' <- nestSnaplet "heist" heist $ heistInit "templates"
    _sesslens' <- nestSnaplet "session" sessLens $ initCookieSessionManager "config/site.txt" "_session" Nothing
    _authlens' <- nestSnaplet "auth" authLens $ initJsonFileAuthManager defAuthSettings sessLens "users.json"
    let sqli = connectSqlite3 "config/wishsys.db"
    _dblens'  <- nestSnaplet "hdbc" dbLens $ hdbcInit sqli
    -- Unable to make hdbc work yet
    -- _authlens' <- nestSnaplet "auth" authLens $ initHdbcAuthManager defAuthSettings sessLens sqli defAuthTable defQueries
    return $ App _heistlens' _authlens' _sesslens' _dblens'

instance HasHeist App where heistLens = subSnaplet heist

main :: IO ()
main = serveSnaplet defaultConfig appInit

mainHandler :: Handler App App ()
mainHandler = do
    H.render "main"

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

renderStuff :: MonadSnap m => String -> m ()
renderStuff text = writeBS (BS.pack text)

imgUrl :: String -> HTML.Html
imgUrl url = HTML.a HTML.! ATTR.href (HTML.toValue url) $ HTML.img HTML.!  ATTR.src (HTML.toValue url) HTML.!  ATTR.width "100" HTML.!  ATTR.height "100"

formatWishAdmin :: Wish -> HTML.Html
formatWishAdmin wish = do
    HTML.tr $ do
              HTML.td $ HTML.toHtml name
              HTML.td $ imgUrl url
              HTML.td $ HTML.toHtml store
  where name  = wishName wish
        url   = wishImg wish
        store = wishStore wish

adminWishTableContent :: [Wish] -> SnapletSplice App App
adminWishTableContent wishList = return . renderHtmlNodes $ do
    HTML.toHtml $ map formatWishAdmin wishList

insertNotification :: String -> HTML.Html
insertNotification msg = HTML.div HTML.! ATTR.id "notification" $ HTML.p $ HTML.toHtml msg

-- Splice to print the notification value
adminInsertNotificationSplice :: (Maybe Wish) -> SnapletSplice App App
adminInsertNotificationSplice (Just (Wish _ name _ _ amount _)) =
    return . renderHtmlNodes $ insertNotification msg
  where msg = ("Satte inn " ++ (show amount) ++ " stykker av '" ++ name ++ "'.")
adminInsertNotificationSplice Nothing                           = return $ []

adminHandler :: Handler App App ()
adminHandler = do
    wish <- insertHandler
    wishList <- getWishes
    renderWithSplices "admin" [("insertNotification", adminInsertNotificationSplice wish)
                              ,("wishTableContent", adminWishTableContent wishList)]

-- Insert handler deals with inserting new wishes into the database.
insertHandler :: Handler App App (Maybe Wish)
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
                          let wish = (Wish 0 whatText urlText storeText amountValue 0)
                          insertWish wish
                          return $ Just wish
         _            ->  return $ Nothing


-- 
formatWish :: Wish -> HTML.Html
formatWish wish = do
    HTML.tr $ do
              HTML.td $ HTML.toHtml name
              HTML.td $ imgUrl url
              HTML.td $ HTML.toHtml store
              HTML.td $ HTML.toHtml remaining
              HTML.td $ registrationForm wishid
  where name      = wishName wish
        url       = wishImg wish
        store     = wishStore wish
        remaining = (wishAmount wish) - (wishBought wish)
        wishid    = (wishId wish)

registrationForm :: Integer -> HTML.Html
registrationForm wishid =
    HTML.form HTML.! ATTR.action "/wishlist" HTML.!  ATTR.method "post" $ do
              HTML.input HTML.! ATTR.type_ "text" HTML.! ATTR.size "2" HTML.!  ATTR.name "amount" HTML.! ATTR.value "0"
              HTML.input HTML.! ATTR.type_ "hidden" HTML.! ATTR.name "wishid" HTML.! ATTR.value (HTML.toValue wishid)
              HTML.input HTML.! ATTR.type_ "submit" HTML.! ATTR.value "Registrer"

wishTableContent :: [Wish] -> SnapletSplice App App
wishTableContent wishList = return . renderHtmlNodes $ do
    HTML.toHtml $ map formatWish wishList

-- Splice to print the notification value
registrationNotificationSplice :: (Maybe (String, Integer)) -> SnapletSplice App App
registrationNotificationSplice (Just (name, amount)) = 
    return . renderHtmlNodes $ insertNotification msg
  where msg = ("Registrerte " ++ (show amount) ++ " stykker av '" ++ name ++ "'.")
registrationNotificationSplice Nothing             = return $ []

-- Handler for the wishlist view. Registers any purchases and displays wish
-- list.
wishViewHandler :: Handler App App ()
wishViewHandler = do
    ret <- purchaseHandler
    wishList <- getWishes
    renderWithSplices "wishlist" [("insertNotification", registrationNotificationSplice ret)
                                 ,("wishTableContent", wishTableContent wishList)]

-- Pull out parameters and perform purchase.
purchaseHandler :: Handler App App (Maybe (String, Integer))
purchaseHandler = do
    wishidParam <- getParam "wishid"
    amountParam <- getParam "amount"
    case (wishidParam, amountParam) of
         (Just wishid, Just amount) -> registerPurchase (read (BS.unpack wishid) ::Integer)
                                                        (read (BS.unpack amount) ::Integer)
            
         _                          -> return Nothing

-- Given a wish id and the amount of items, subtract this wish' remaining
-- amount.
registerPurchase :: Integer -> Integer -> Handler App App (Maybe (String, Integer))
registerPurchase wishid amount = do
    wish <- getWish wishid
    let bought = wishBought wish
    updateWish wishid (bought + amount)
    return $ Just ((wishName wish), amount)

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
