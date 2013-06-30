{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Handler.Login

getHomeR :: Handler Html
getHomeR = do
    loginForm <- generateLoginForm
    (formWidget, enctype) <- generateFormPost loginForm
    defaultLayout $ do
        setTitleI MsgHomeTitle
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    render <- getMessageRender
    loginForm <- generateLoginForm
    ((result, _), _) <- runFormPost loginForm
    case result of 
        FormSuccess (accessLevel, name, password) -> do
            let loginName = getLoginName accessLevel name
            doLogin loginName password
            wl <- runDB $ selectList [WishlistName ==. name] [LimitTo 1]
            case wl of
                [] -> do
                    setMessage $ toHtml $ render MsgWishListNotFound
                    redirect $ HomeR
                (Entity wid _):_ -> do
                    redirect $ (WishListR wid accessLevel)
        _ -> do
            setMessage $ toHtml $ render MsgErrorDuringLogin
            redirect $ HomeR




generateLoginForm :: Handler (Form (AccessLevel, Text, Text))
generateLoginForm = do
    render <- getMessageRender
    let accessLevels = (map (\x -> (render x, x)) $ [minBound..maxBound]) :: [(Text, AccessLevel)]
    return $ renderBootstrap $ (,,)
            <$> areq (radioFieldList accessLevels) "" (Just Guest)
            <*> areq textField (fieldSettingsLabel MsgLoginFormListName) Nothing
            <*> areq passwordField (fieldSettingsLabel MsgLoginFormPassword) Nothing
