{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.WishList where

import Import
import Yesod.Auth

getWishListR :: WishlistId -> Handler RepHtml
getWishListR listId = do
    list <- runDB $ get listId
    wishes <- runDB $ selectList ([WishWlist ==. listId] :: [Filter Wish]) []
    userId <- requireAuthId
    let numWishes = length wishes
    let handlerName = "getWishListR" :: Text
    let widget = getWidget list wishes userId
    defaultLayout $ do
      setTitleI MsgWishListTitle
      widget

getWidget :: Maybe Wishlist -> [Entity Wish] -> UserId -> Widget
getWidget Nothing _ _ = $(widgetFile "impossible")
getWidget (Just (Wishlist _ ownerId guestId)) wishes userId =
  if userId == guestId
  then $(widgetFile "wishlist_guest")
  else if userId == ownerId
       then $(widgetFile "wishlist_owner")
       else $(widgetFile "impossible")
