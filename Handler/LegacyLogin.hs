{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.LegacyLogin where

import Import
import Handler.Login
import Data.Text


postLegacyLoginR :: Handler Html
postLegacyLoginR = do
    render <- getMessageRender
    ((result, _), _) <- runFormPost legacyForm
    case result of
        FormSuccess (name, password) -> do
            case name of
                "gjest" -> loginUser "cogm" (append name password) Guest
                "cogm" -> loginUser "cogm" (append name password) Admin
                _ -> do
                    setMessage $ toHtml $ render MsgErrorLegacyLoginName
                    redirect LegacyLoginR
        _ -> do
            setMessage $ toHtml $ render MsgErrorDuringLogin
            redirect $ LegacyLoginR

getLegacyLoginR :: Handler Html
getLegacyLoginR = do
    (formWidget, enctype) <- generateFormPost legacyForm
    defaultLayout $(widgetFile "legacylogin")

legacyForm :: Form (Text, Text)
legacyForm = renderBootstrap $ (,)
    <$> areq textField "brukernavn" Nothing
    <*> areq passwordField "passord" Nothing

loginUser :: Text -> Text -> AccessLevel -> Handler Html
loginUser listName password accessLevel = do
    let loginName = getLoginName accessLevel listName
    doLogin loginName password LegacyLoginR
    redirect $ WishListR listName accessLevel
