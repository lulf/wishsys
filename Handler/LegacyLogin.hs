{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.LegacyLogin where

import Import
import Handler.Login
import Data.Text

postLegacyLoginR :: Handler Html
postLegacyLoginR = do
    (name, password) <- runInputPost $ (,)
                          <$> ireq textField "login"
                          <*> ireq passwordField "password"
    case name of
        "gjest" -> loginUser "cogm" (append name password) Guest
        "cogm" -> loginUser "cogm" (append name password) Admin
        _ -> redirectToLegacyPage

getListId :: Text -> Handler (Maybe WishlistId)
getListId name = do
  lists <- runDB $ selectList [WishlistName ==. name] []
  case lists of
    [] -> return $ Nothing
    ((Entity listId _):_) -> return $ Just listId

redirectToLegacyPage :: Handler Html
redirectToLegacyPage = redirect ("http://wishsys.dimling.net:8888" :: String)

loginUser :: Text -> Text -> AccessLevel -> Handler Html
loginUser listName password accessLevel = do
    maybeListId <- getListId listName
    case maybeListId of
        Nothing -> redirectToLegacyPage
        Just listId -> do
            let loginName = getLoginName accessLevel listName
            doLogin loginName password
            redirect $ WishListR listId accessLevel
