{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Register where

import Import
import qualified Yesod.Auth.HashDB as H
import Yesod.Auth

getRegisterR :: Handler RepHtml
getRegisterR = do
    (formWidget, enctype) <- generateFormPost registerForm
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Register"
        $(widgetFile "register")

postRegisterR :: Handler RepHtml
postRegisterR = do
    ((result, _), _) <- runFormPost registerForm
    case result of
        FormSuccess (name, adminPassword, guestPassword) -> do
            user <- liftIO $ H.setPassword adminPassword (User name "" "")
            _ <- runDB $ insert user
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
