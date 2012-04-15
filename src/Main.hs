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
import            Render

-- Third party.
import qualified  Data.ByteString.Char8 as BS (unpack)
import            Snap
import            Database.HDBC.Sqlite3
import            Snap.Snaplet.Hdbc
import            Snap.Snaplet.Auth hiding (siteKey)
import            Snap.Snaplet.Heist as H
import            Snap.Snaplet.Auth.Backends.JsonFile
import            Snap.Snaplet.Session.Backends.CookieSession
import            Snap.Util.FileServe
import            Text.Blaze.Renderer.XmlHtml
import qualified  Data.Text

appInit :: SnapletInit App App
appInit = makeSnaplet "wishsys" "Wish list application" Nothing $ do
    addAuthRoutes [ ("wishlist", wishViewHandler [], guestUsers)
                  , ("wishlist/inserted", wishInsertedViewHandler, guestUsers)
                  , ("wishlist/insert", wishInsertHandler, guestUsers)
                  , ("admin", adminHandler [], adminUsers)
                  , ("admin/insert", adminInsertHandler, adminUsers)
                  , ("admin/inserted", adminInsertedHandler, adminUsers)
                  , ("admin/edit", adminEditHandler, adminUsers)
                  , ("admin/edited", adminEditedHandler, adminUsers) ]
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

-- Splice for displaying the admin table
adminWishTableSplice :: [Wish] -> SnapletSplice App App
adminWishTableSplice wishList = return . renderHtmlNodes $ wishEditFormTable wishList

-- Insert handler, inserts wish and redirects to view page again.
adminInsertHandler :: Handler App App ()
adminInsertHandler = do
    insertHandler
    redirect' "/admin/inserted" 303

-- Wraps adminHandler and adds a notification message
adminInsertedHandler :: Handler App App ()
adminInsertedHandler =
    adminHandler [ ("notification", notificationSplice "Ditt ønske ble satt inn i databasen!") ]

-- Edit handler, updates wish entry and redirects
adminEditHandler :: Handler App App ()
adminEditHandler = do
    editHandler
    redirect' "/admin/edited" 303

-- Wraps adminHandler and adds a notification message
adminEditedHandler :: Handler App App ()
adminEditedHandler = do
    adminHandler [("notification", notificationSplice "Ønsket ble oppdatert!")]

-- Displays the wish list as editable form entries
adminHandler :: [(Data.Text.Text, SnapletSplice App App)] -> Handler App App ()
adminHandler splices = do
    wishList <- getWishes
    renderWithSplices "admin" (splices ++ [("wishTableContent", adminWishTableSplice wishList)])

-- Insert handler deals with inserting new wishes into the database.
editHandler :: Handler App App ()
editHandler = do
    idParam <- getParam "wishId"
    nameParam <- getParam "wishName"
    urlParam <- getParam "wishUrl"
    storeParam <- getParam "wishStore"
    amountParam <- getParam "wishAmount"
    deleteFlagParam <- getParam "wishDeleteFlag"
    case (idParam, nameParam, urlParam, storeParam, amountParam, deleteFlagParam) of
        (Just wishid, _, _, _, _,
         Just "delete") -> do deleteWish (read (BS.unpack wishid) :: Integer)
        (Just wishid,
         Just name,
         Just url,
         Just store,
         Just amount,
         _ ) -> do let wish = (Wish (read (BS.unpack wishid) :: Integer)
                                    (BS.unpack name)
                                    (BS.unpack url)
                                    (BS.unpack store)
                                    (read (BS.unpack amount) :: Integer)
                                    0)
                   updateWish wish
        _   -> return ()

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


-- Splice displaying a wish list
wishTableSplice :: [Wish] -> SnapletSplice App App
wishTableSplice wishList = return . renderHtmlNodes $ wishTable wishList

-- Splice to print the notification value
notificationSplice :: String -> SnapletSplice App App
notificationSplice msg =
    return . renderHtmlNodes $ insertNotification msg

-- Handler for the wishlist view. Registers any purchases and displays wish
-- list.
wishViewHandler :: [(Data.Text.Text, SnapletSplice App App)] -> Handler App App ()
wishViewHandler splices = do
    wishList <- getWishes
    renderWithSplices "wishlist" (splices ++ [ ("wishTableContent", wishTableSplice wishList) ])

-- Handler for the case where a wish was inserted
wishInsertedViewHandler :: Handler App App ()
wishInsertedViewHandler =
    wishViewHandler [ ("notification", notificationSplice "Ditt kjøp ble registrert!") ]

-- Handler that performs the insert and does a redirect
wishInsertHandler :: Handler App App ()
wishInsertHandler = do
    purchaseHandler
    redirect' "/wishlist/inserted" 303


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
registerPurchase :: WishID -> Integer -> Handler App App (Maybe (String, Integer))
registerPurchase wishid amount = do
    wish <- getWish wishid
    let bought = wishBought wish
    updateWishBought wishid (bought + amount)
    return $ Just ((wishName wish), amount)
