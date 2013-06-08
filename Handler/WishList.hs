{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.WishList where

import Import
import Yesod.Auth

getWishListR :: WishlistId -> Handler RepHtml
getWishListR listId = do
    maybeList <- runDB $ get listId
    userId <- requireAuthId
    wishes <- runDB $ selectList ([WishWlist ==. listId] :: [Filter Wish]) []
    case maybeList of
        Just list@(Wishlist _ ownerId guestId) ->
          if userId == guestId
          then getGuestWishList list wishes
          else if userId == ownerId
               then getOwnerWishList list wishes
               else redirect HomeR
        _ -> redirect HomeR

getOwnerWishList :: Wishlist -> [Entity Wish] -> Handler RepHtml
getOwnerWishList listId wishes = do
  defaultLayout $ do
      setTitleI MsgWishListTitle
      $(widgetFile "wishlist_owner")

getGuestWishList :: Wishlist -> [Entity Wish] -> Handler RepHtml
getGuestWishList listId wishes = do
  defaultLayout $ do
      setTitleI MsgWishListTitle
      $(widgetFile "wishlist_guest")

createWishEditFormWidget :: WishId -> Wish -> Widget
createWishEditFormWidget _ _ = [whamlet|
<p>HELLO</p>
|]
