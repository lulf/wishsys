{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Yesod.Auth
import Import

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, enctype) <- generateFormPost guestForm
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Wish sys"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), enctype) <- runFormPost guestForm
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

guestForm :: Form (Text, Text)
guestForm = renderBootstrap $ (,)
    <$> areq textField "Name of wish list" Nothing
    <*> areq textField "Guest password" Nothing
