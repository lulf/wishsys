{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Yesod.Auth
import Import
import Data.Text (pack)
import Control.Arrow

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
    ((result, formWidget), enctype) <- runFormPost guestForm
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

guestForm :: Form (Text, Text, AccessLevel)
guestForm = renderBootstrap $ (,,)
    <$> areq textField "Name of wish list" Nothing
    <*> areq textField "Password" Nothing
    <*> areq (radioFieldList accessLevels) "" Nothing
  where accessLevels :: [(Text, AccessLevel)]
        accessLevels = map (pack . show &&& id) $ [minBound..maxBound]
