{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.WishListView where


import Import

getWishListViewR :: Handler RepHtml
getWishListViewR = do
    ownerWishes <- runDB $ selectList ([] :: [Filter Wishlist]) []
    viewableWishes <- runDB $ selectList ([] :: [Filter Wishlist]) []
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Wishlist overview"
        $(widgetFile "wishlist_overview")
