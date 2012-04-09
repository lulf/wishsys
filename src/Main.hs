{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

-- My own modules
import            Config
import            Auth
import            Common
import            Persistence

-- Third party.
import qualified  Data.ByteString.Char8 as BS (unpack)
import            Snap
import            Database.HDBC.Sqlite3
import            Snap.Snaplet.Hdbc
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Heist as H
import            Snap.Snaplet.Auth.Backends.JsonFile
import            Snap.Snaplet.Session.Backends.CookieSession
import            Snap.Util.FileServe
import qualified  Text.Blaze.Html5 as HTML
import qualified  Text.Blaze.Html5.Attributes as ATTR
import            Text.Blaze.Renderer.XmlHtml

appInit :: SnapletInit App App
appInit = makeSnaplet "wishsys" "Wish list application" Nothing $ do
    addAuthRoutes [ ("wishlist", wishViewHandler, guestUsers)
                  , ("admin", adminHandler, adminUsers) ]
    addRoutes [ ("", mainHandler)
              , ("test", heistServe)
              , ("public/stylesheets", serveDirectory "public/stylesheets")
              , ("login", loginHandler)
              , ("logout", logoutHandler) ]
              
    _heistlens' <- nestSnaplet "heist" heist $ heistInit "templates"
    _sesslens' <- nestSnaplet "session" sessLens $ initCookieSessionManager siteKey "_session" Nothing
    _authlens' <- nestSnaplet "auth" authLens $ initJsonFileAuthManager defAuthSettings sessLens userDB
    let sqli = connectSqlite3 wishDB
    _dblens'  <- nestSnaplet "hdbc" dbLens $ hdbcInit sqli
    -- Unable to make hdbc work yet
    -- _authlens' <- nestSnaplet "auth" authLens $ initHdbcAuthManager defAuthSettings sessLens sqli defAuthTable defQueries
    addSplices [ ("ifLoggedIn", ifLoggedIn authLens)
               , ("ifLoggedOut", ifLoggedOut authLens)
               , ("loggedInUser", loggedInUser authLens) ]

    return $ App _heistlens' _authlens' _sesslens' _dblens'

main :: IO ()
main = serveSnaplet defaultConfig appInit

-- Render the login form page
mainHandler :: Handler App App ()
mainHandler = with authLens $ loginForm False


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
    renderWithSplices "admin" [("notification", adminInsertNotificationSplice wish)
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
    renderWithSplices "wishlist" [("notification", registrationNotificationSplice ret)
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
