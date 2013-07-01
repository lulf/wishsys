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
                _ -> redirect $ LegacyLoginR
        _ -> do
            setMessage $ toHtml $ render MsgErrorDuringLogin
            redirect $ LegacyLoginR

legacyForm :: Form (Text, Text)
legacyForm = renderBootstrap $ (,)
    <$> areq textField "login" Nothing
    <*> areq passwordField "password" Nothing

getListId :: Text -> Handler (Maybe WishlistId)
getListId name = do
  lists <- runDB $ selectList [WishlistName ==. name] []
  case lists of
    [] -> return $ Nothing
    ((Entity listId _):_) -> return $ Just listId

loginUser :: Text -> Text -> AccessLevel -> Handler Html
loginUser listName password accessLevel = do
    maybeListId <- getListId listName
    case maybeListId of
        Nothing -> redirect $ LegacyLoginR
        Just listId -> do
            let loginName = getLoginName accessLevel listName
            doLogin loginName password
            redirect $ WishListR listId accessLevel
