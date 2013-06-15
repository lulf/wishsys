{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Yesod.Auth
import Yesod.Auth.Message
import Yesod.Auth.HashDB (validateUser)
import Import
import Data.Text (pack, unpack)
import Control.Arrow
import Data.Maybe

getHomeR :: Handler RepHtml
getHomeR = do
    loginForm <- generateLoginForm
    (formWidget, enctype) <- generateFormPost loginForm
    defaultLayout $ do
        setTitleI MsgHomeTitle
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    render <- getMessageRender
    loginForm <- generateLoginForm
    ((result, _), _) <- runFormPost loginForm
    case result of 
        FormSuccess (accessLevel, name, password) -> do
            let loginName = case accessLevel of
                                Admin -> pack $ "admin_" ++ (unpack name)
                                Guest -> pack $ "guest_" ++ (unpack name)
            doLogin loginName password
            wl <- runDB $ selectList [WishlistName ==. name] [LimitTo 1]
            case wl of
                [] -> do
                    setMessage $ toHtml $ render MsgWishListNotFound
                    redirect $ HomeR
                (Entity wid _):_ -> do
                    redirect $ (WishListR wid)
        _ -> do
            setMessage $ toHtml $ render MsgErrorDuringLogin
            redirect $ HomeR


doLogin :: Text -> Text -> Handler ()
doLogin mu mp = do
    let uid = Just $ UniqueUser mu 
    isValid <- fromMaybe (return False) (validateUser <$> uid <*> (Just mp))
    if isValid 
       then setCreds False $ Creds "hashdb" mu []
       else do
            render <- getMessageRender
            loginErrorMessage (AuthR LoginR) (render MsgInvalidUserOrPassword)

generateLoginForm :: Handler (Form (AccessLevel, Text, Text))
generateLoginForm = do
    render <- getMessageRender
    let accessLevels = (map (\x -> (render x, x)) $ [minBound..maxBound]) :: [(Text, AccessLevel)]
    return $ renderBootstrap $ (,,)
            <$> areq (radioFieldList accessLevels) "" Nothing
            <*> areq textField (fieldSettingsLabel MsgLoginFormListName) Nothing
            <*> areq passwordField (fieldSettingsLabel MsgLoginFormPassword) Nothing
