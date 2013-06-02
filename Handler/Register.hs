{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Register where

import Import
import qualified Yesod.Auth.HashDB as H
import Yesod.Auth
import qualified Data.Text as T

getRegisterR :: Handler RepHtml
getRegisterR = do
    (formWidget, enctype) <- generateFormPost registerForm
    defaultLayout $ do
        setTitleI MsgRegisterTitle
        $(widgetFile "register")

postRegisterR :: Handler RepHtml
postRegisterR = do
    ((result, _), _) <- runFormPost registerForm
    case result of
        FormSuccess (name, adminPassword, guestPassword) -> do
            let adminName = T.pack ("admin_" ++ (T.unpack name))
            let guestName = T.pack ("guest_" ++ (T.unpack name))
            adminUser <- liftIO $ H.setPassword adminPassword (User adminName "" "")
            guestUser <- liftIO $ H.setPassword guestPassword (User guestName "" "")
            adminId <- runDB $ insert adminUser
            guestId <- runDB $ insert guestUser
            _ <- runDB $ insert (Wishlist name adminId guestId)
            setMessage "Wish list successfully created!" -- i18n
            redirect $ AuthR LoginR
        _ -> do
            setMessage "Error creating with list " -- i18n
            redirect $ RegisterR

registerForm :: Form (Text, Text, Text)
registerForm = renderBootstrap $ (,,)
    <$> areq textField "Wish list name" Nothing
    <*> areq passwordField "Admin password" Nothing
    <*> areq passwordField "Guest password" Nothing
