{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.JsonWishListList where

import Import

getJsonWishListListR :: Handler Html
getJsonWishListListR = do
    defaultLayout $ do
        setTitleI MsgHomeTitle
        $(widgetFile "homepage")
