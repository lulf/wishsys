{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Yesod.Auth
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
    loginForm <- generateLoginForm
    ((result, _), _) <- runFormPost loginForm
    case result of 
        FormSuccess (name, password, accessLevel) -> do
            let loginName = case accessLevel of
                                Admin -> pack $ "admin_" ++ (unpack name)
                                Guest -> pack $ "guest_" ++ (unpack name)
            doLogin loginName password
            wl <- runDB $ selectList [WishlistName ==. name] [LimitTo 1]
            case wl of
                [] -> do
                    setMessage "Wish list not found"
                    redirect $ HomeR
                (Entity wid _):_ -> do
                    redirect $ (WishListR wid)
        _ -> do
            setMessage "Error login in" -- i18n
            redirect $ HomeR


doLogin :: Text -> Text -> Handler ()
doLogin mu mp = do
    let uid = Just $ UniqueUser mu 
    isValid <- fromMaybe (return False) (validateUser <$> uid <*> (Just mp))
    if isValid 
       then setCreds False $ Creds "hashdb" mu []
       else loginErrorMessage (AuthR LoginR) "Invalid username/password"

generateLoginForm :: Handler (Form (Text, Text, AccessLevel))
generateLoginForm = do
    render <- getMessageRender
    let accessLevels = (map (\x -> (render x, x)) $ [minBound..maxBound]) :: [(Text, AccessLevel)]
    return $ renderBootstrap $ (,,)
            <$> areq textField (fieldSettingsLabel MsgLoginFormListName) Nothing
            <*> areq passwordField (fieldSettingsLabel MsgLoginFormPassword) Nothing
            <*> areq (radioFieldList accessLevels) "" Nothing
