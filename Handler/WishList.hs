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
  (wishRegisterWidget, enctype) <- generateFormPost (wishRegisterForm listId)
  defaultLayout $ do
      setTitleI MsgWishListTitle
      $(widgetFile "wishlist_owner")

getGuestWishList :: Wishlist -> [Entity Wish] -> Handler RepHtml
getGuestWishList listId wishes = do
  defaultLayout $ do
      setTitleI MsgWishListTitle
      $(widgetFile "wishlist_guest")

wishRegisterForm :: WishlistId -> Form (Wish)
wishRegisterForm listId = renderBootstrap $ Wish
    <$> areq textField (fieldSettingsLabel MsgWishRegisterFormName) Nothing
    <*> areq textField (fieldSettingsLabel MsgWishRegisterFormImage) Nothing
    <*> areq textField (fieldSettingsLabel MsgWishRegisterFormStores) Nothing
    <*> areq intField (fieldSettingsLabel MsgWishRegisterFormAmount) Nothing
    <*> areq hiddenField "" Nothing
    <*> areq wishListIdField "" Nothing

wishListIdField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m WishlistId
wishListIdField = Field
    { fieldParse = \rawVals _ ->
        case rawVals of
            [a] -> return $ Right $ Just (read (unpack a) :: WishlistId)
            _ -> return $ Left "You must enter a valid id"
    , fieldView = \idAttr nameAttr _ eResult isReq -> [whamlet|
<input id=#{idAttr} name=#{nameAttr} type=hidden>
|]
    }

wishEditWidget :: WishId -> Wish -> Widget
wishEditWidget _ _ = [whamlet|
<p>HELLO</p>
|]
