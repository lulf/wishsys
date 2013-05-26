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
        FormSuccess (email, password) -> do
            user <- liftIO $ H.setPassword password (User email "" "")
            _ <- runDB $ insert user
            setMessage "You have been registered!" -- I18n
            redirect $ AuthR LoginR
        _ -> do
            setMessage "Error registering user" -- I18n
            redirect $ RegisterR

registerForm :: Form (Text, Text)
registerForm = renderBootstrap $ (,)
    <$> areq textField "Email" Nothing
    <*> areq passwordField "Password" Nothing
