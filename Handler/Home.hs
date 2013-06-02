{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Yesod.Auth
import Yesod.Auth.HashDB (validateUser)
import Import
import Data.Text (pack, unpack)
import Control.Arrow
import Data.Maybe

data AccessLevel = Guest | Admin
    deriving (Show, Eq, Enum, Bounded)

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, enctype) <- generateFormPost guestForm
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Wish sys"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, _), _) <- runFormPost guestForm
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
       then setCreds True $ Creds "hashdb" mu []
       else loginErrorMessage (AuthR LoginR) "Invalid username/password"
    --defaultLayout $ do
    --    aDomId <- newIdent
    --    setTitle "Welcome To Yesod!"
    --    $(widgetFile "homepage")
            

guestForm :: Form (Text, Text, AccessLevel)
guestForm = renderBootstrap $ (,,)
    <$> areq textField "Name of wish list" Nothing
    <*> areq textField "Password" Nothing
    <*> areq (radioFieldList accessLevels) "" Nothing
  where accessLevels :: [(Text, AccessLevel)]
        accessLevels = map (pack . show &&& id) $ [minBound..maxBound]
