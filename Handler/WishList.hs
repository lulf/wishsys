{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.WishList where

import Import
import Yesod.Auth
import Data.Text (unpack)

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
               then getOwnerWishList listId wishes
               else redirect HomeR
        _ -> redirect HomeR

getOwnerWishList :: WishlistId -> [Entity Wish] -> Handler RepHtml
getOwnerWishList listId wishes = do
    (wishRegisterWidget, enctype) <- generateFormPost $ wishRegisterForm listId
    defaultLayout $ do
        setTitleI MsgWishListTitle
        $(widgetFile "wishlist_owner")

getGuestWishList :: Wishlist -> [Entity Wish] -> Handler RepHtml
getGuestWishList listId wishes = do
  defaultLayout $ do
      setTitleI MsgWishListTitle
      $(widgetFile "wishlist_guest")

postWishListR :: WishlistId -> Handler RepHtml
postWishListR listId = do
    render <- getMessageRender
    ((result, _), _) <- runFormPost $ wishRegisterForm listId
    case result of
        FormSuccess (wish) -> do
            runDB $ insert wish
            setMessage $ toHtml $ render MsgRegisterWishWishAdded
            redirect $ (WishListR listId)
        _ -> do
            setMessage $ toHtml $ render MsgRegisterWishErrorAdding
            redirect $ (WishListR listId)


wishRegisterForm :: WishlistId -> Form (Wish)
wishRegisterForm listId = renderBootstrap $ Wish
    <$> areq textField (fieldSettingsLabel MsgWishRegisterFormName) Nothing
    <*> areq textField (fieldSettingsLabel MsgWishRegisterFormImage) Nothing
    <*> areq textField (fieldSettingsLabel MsgWishRegisterFormStores) Nothing
    <*> areq intField (fieldSettingsLabel MsgWishRegisterFormAmount) Nothing
    <*> pure 0
    <*> pure listId

wishEditWidget :: WishId -> Wish -> Widget
wishEditWidget _ _ = [whamlet|
<p>HELLO</p>
|]
