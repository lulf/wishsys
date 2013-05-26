{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.WishListView where

import Import
import Yesod.Auth

wishListForm :: Form Wishlist
wishListForm = renderBootstrap $ Wishlist
    <$> areq textField "Name" Nothing
    <*> areq textField "Guest password" Nothing
    <*> userId
  where userId = lift $ requireAuthId

getWishListViewR :: Handler RepHtml
getWishListViewR = do
    (formWidget, enctype) <- generateFormPost wishListForm
    userId <- requireAuthId
    wishLists <- runDB $ selectList ([WishlistOwner ==. userId] :: [Filter Wishlist]) []
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Wishlist overview"
        $(widgetFile "wishlist_overview")

postWishListViewR :: Handler RepHtml
postWishListViewR = do
    ((result, _), _) <- runFormPost wishListForm
    case result of
        FormSuccess list -> do
            wishListId <- runDB $ insert list
            setMessage "Your wish list has been created" -- I18n
            redirect $ WishListViewR
        _ -> do
            setMessage "Error creating new wish list" -- I18n
            redirect $ WishListViewR
