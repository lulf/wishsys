{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.WishList where


import Import

getWishListR :: Handler RepHtml
getWishListR = do
    wishes <- runDB $ selectList ([] :: [Filter Wish]) []
    let numWishes = length wishes
    let handlerName = "getWishListR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Foo"
        $(widgetFile "wishlist")
