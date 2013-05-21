{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.WishList where


import Import

getWishListR :: Text -> Handler RepHtml
getWishListR listName = do
    wishes <- runDB $ selectList ([] :: [Filter Wish]) []
    let numWishes = length wishes
    let handlerName = "getWishListR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Wish list"
        $(widgetFile "wishlist")
