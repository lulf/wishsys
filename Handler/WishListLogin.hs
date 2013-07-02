{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.WishListLogin where

import Import
import Handler.Login

getWishlist :: Text -> Handler (Wishlist)
getWishlist urlName = do
    render <- getMessageRender
    maybeWishList <- runDB $ selectList [WishlistUrlName ==. urlName] [LimitTo 1]
    case maybeWishList of
        [] -> do setMessage $ toHtml $ render MsgWishListNotFound
                 redirect $ HomeR
        (Entity _ wishList):_ -> return $ wishList

getWishListLoginR :: Text -> AccessLevel -> Handler Html
getWishListLoginR urlName accessLevel = do
    wishList <- getWishlist urlName
    (formWidget, enctype) <- generateFormPost $ loginForm urlName accessLevel
    defaultLayout $ do
        setTitleI MsgHomeTitle
        $(widgetFile "wishlist_login")

postWishListLoginR :: Text -> AccessLevel -> Handler Html
postWishListLoginR urlName accessLevel = do
    render <- getMessageRender
    ((result, _), _) <- runFormPost $ loginForm urlName accessLevel
    case result of 
        FormSuccess (_, _, password) -> do
            let loginName = getLoginName accessLevel urlName
            doLogin loginName password $ WishListLoginR urlName accessLevel
            redirect $ WishListR urlName accessLevel
        _ -> do
            setMessage $ toHtml $ render MsgErrorDuringLogin
            redirect $ WishListLoginR urlName accessLevel



loginForm :: Text -> AccessLevel -> Form (AccessLevel, Text, Text)
loginForm urlName accessLevel = renderBootstrap $ (,,)
            <$> pure accessLevel
            <*> pure urlName
            <*> areq passwordField (fieldSettingsLabel MsgLoginFormPassword) Nothing
