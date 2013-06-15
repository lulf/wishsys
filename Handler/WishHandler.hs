{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.WishHandler where

import Import
import Handler.WishList
import Yesod.Auth

postWishHandlerR :: WishlistId -> WishId -> Handler RepHtml
postWishHandlerR listId wishId = do
    maybeList <- runDB $ get listId
    userId <- requireAuthId
    case maybeList of
        Just list@(Wishlist _ ownerId guestId) ->
          if userId == guestId
          then postGuestWishHandlerR listId wishId
          else if userId == ownerId
               then postOwnerWishHandlerR listId wishId
               else redirect HomeR
        _ -> redirect $ WishListR listId

postGuestWishHandlerR :: WishlistId -> WishId -> Handler RepHtml
postGuestWishHandlerR listId wishId = do
    render <- getMessageRender
    ((result, _), _) <- runFormPost $ wishGuestForm
    case result of
        FormSuccess (numPurchased) -> do
            runDB $ update wishId [WishBought +=. numPurchased]
        _ -> setMessage $ toHtml $ render MsgRegisterPurchasedError

    redirect $ WishListR listId

postOwnerWishHandlerR :: WishlistId -> WishId -> Handler RepHtml
postOwnerWishHandlerR listId wishId = do
    render <- getMessageRender
    ((updateResult, _), _) <- runFormPost $ wishOwnerForm listId Nothing
    wasUpdated <- updateIfSuccess updateResult wishId
    if wasUpdated
      then return $ ()
      else do
        ((deleteResult, _), _) <- runFormPost $ deleteWishForm wishId
        wasDeleted <- deleteIfSuccess deleteResult wishId
        if wasDeleted
          then return $ ()
          else setMessage $ toHtml $ render MsgRegisterWishErrorChangingWish
    redirect $ (WishListR listId)

updateIfSuccess :: FormResult Wish -> WishId -> Handler (Bool)
updateIfSuccess result wishId  = do
    render <- getMessageRender
    case result of
        FormSuccess (wish) -> do
            runDB $ replace wishId wish
            setMessage $ toHtml $ render MsgRegisterWishWishUpdated
            return $ True
        _ -> return $ False

deleteIfSuccess :: FormResult WishId -> WishId -> Handler (Bool)
deleteIfSuccess result wishId = do
    render <- getMessageRender
    case result of
        FormSuccess (_) -> do
            runDB $ delete wishId
            setMessage $ toHtml $ render MsgRegisterWishWishDeleted
            return $ True
        _ -> return $ False
