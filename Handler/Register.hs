{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Register where

import Import
import qualified Yesod.Auth.HashDB as H
import qualified Data.Text as T
import Handler.Login

getRegisterR :: Handler Html
getRegisterR = do
    (formWidget, enctype) <- generateFormPost registerForm
    defaultLayout $ do
        setTitleI MsgRegisterTitle
        $(widgetFile "register")

redirectIfListExists :: Text -> Text -> Handler ()
redirectIfListExists name urlName = do
    render <- getMessageRender
    exists <- runDB $ selectList ([WishlistName ==. name] ||. [WishlistUrlName ==. urlName]) []
    if length exists > 0
      then do
        setMessage $ toHtml $ render MsgWishListAlreadyExists
        redirect $ RegisterR
      else
        return $ ()

postRegisterR :: Handler Html
postRegisterR = do
    render <- getMessageRender
    ((result, _), _) <- runFormPost registerForm
    case result of
        FormSuccess (name, urlName, adminPassword, guestPassword) -> do
            redirectIfListExists name urlName
            let adminName = T.pack ("admin_" ++ (T.unpack name))
            let guestName = T.pack ("guest_" ++ (T.unpack name))
            adminUser <- liftIO $ H.setPassword adminPassword (User adminName "" "")
            guestUser <- liftIO $ H.setPassword guestPassword (User guestName "" "")
            adminId <- runDB $ insert adminUser
            guestId <- runDB $ insert guestUser
            _ <- runDB $ insert (Wishlist name urlName adminId guestId)
            setMessage $ toHtml $ render MsgWishListCreateSuccess
            doLogin adminName adminPassword HomeR
            redirect $ WishListR urlName Admin
        _ -> do
            setMessage $ toHtml $ render MsgWishListCreateError
            redirect $ RegisterR

registerForm :: Form (Text, Text, Text, Text)
registerForm = renderBootstrap $ (,,,)
    <$> areq textField (fieldSettingsLabel MsgRegisterFormListName) Nothing
    <*> areq textField (fieldSettingsLabel MsgRegisterFormShortName) Nothing  -- TODO: Fix URL and use URL type to santize input
    <*> areq passwordField (fieldSettingsLabel MsgRegisterFormAdminPassword) Nothing
    <*> areq passwordField (fieldSettingsLabel MsgRegisterFormGuestPassword) Nothing
