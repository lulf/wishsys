{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Main where

-- My own modules
import           Auth
import           Common
import           Config
import           SnapConfig
import           Persistence
import           Render

-- Third party.
import qualified Data.ByteString.Char8                       as BS (unpack)
import qualified Data.Text
import           Data.Maybe
import           Database.HDBC.Sqlite3
import           Snap
import           Snap.Snaplet.Auth                           hiding (siteKey)
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Heist                          as H
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Text.Blaze.Renderer.XmlHtml

import           System.Environment

appInit :: WishsysConfig -> SnapletInit App App
appInit config = makeSnaplet "wishsys" "Wish list application" Nothing $ do
    addAuthRoutes [ ("wishlist", wishViewHandler [], guestUsers config)
                  , ("wishlist/inserted", wishInsertedViewHandler, guestUsers config)
                  , ("wishlist/insert", wishInsertHandler, guestUsers config)
                  , ("admin", adminHandler [], adminUsers config)
                  , ("admin/insert", adminInsertHandler, adminUsers config)
                  , ("admin/inserted", adminInsertedHandler, adminUsers config)
                  , ("admin/edit", adminEditHandler, adminUsers config)
                  , ("admin/edited", adminEditedHandler, adminUsers config) ]
    addRoutes [ ("", mainHandler)
              , ("test", heistServe)
              , ("public/stylesheets", serveDirectory "public/stylesheets")
              , ("login", loginHandler config)
              , ("logout", logoutHandler) ]

    _heistlens' <- nestSnaplet "heist" heist $ heistInit "templates"
    _sesslens' <- nestSnaplet "session" sessLens $ initCookieSessionManager (siteKey config) "_session" Nothing
    _authlens' <- nestSnaplet "auth" authLens $ initJsonFileAuthManager defAuthSettings sessLens $ userDB config
    let sqli = connectSqlite3 $ wishDB config
    _dblens'  <- nestSnaplet "hdbc" dbLens $ hdbcInit sqli
    -- Unable to make hdbc work yet
    -- _authlens' <- nestSnaplet "auth" authLens $ initHdbcAuthManager defAuthSettings sessLens sqli defAuthTable defQueries
    addSplices [ ("ifLoggedIn", ifLoggedIn authLens)
               , ("ifLoggedOut", ifLoggedOut authLens)
               , ("loggedInUser", loggedInUser authLens) ]

    return $ App _heistlens' _authlens' _sesslens' _dblens'

main :: IO ()
main = do
  args <- getArgs
  let configFile = findConfigFile args
  config <- fromConfigFile $ fromMaybe "config.cfg" configFile
  serveSnaplet defaultConfig (appInit config)

findConfigFile :: [String] -> Maybe String
findConfigFile [] = Nothing
findConfigFile (_:[]) = Nothing
findConfigFile (arg:file:args) =
  if arg == "--config"
  then Just file
  else findConfigFile (file:args)

-- Render the login form page
mainHandler :: Handler App App ()
mainHandler = with authLens $ loginForm False

-- Splice for displaying the admin table
adminWishTableSplice :: [Wish] -> SnapletISplice App
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
adminHandler :: [(Data.Text.Text, SnapletISplice App)] -> Handler App App ()
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
wishTableSplice :: [Wish] -> SnapletISplice App
wishTableSplice wishList = return . renderHtmlNodes $ wishTable wishList

-- Splice to print the notification value
notificationSplice :: String -> SnapletISplice App
notificationSplice msg =
    return . renderHtmlNodes $ insertNotification msg

-- Handler for the wishlist view. Registers any purchases and displays wish
-- list.
wishViewHandler :: [(Data.Text.Text, SnapletISplice App)] -> Handler App App ()
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
