{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.WishList where

import Import
import Yesod.Auth
import Data.Text (unpack)
import Data.Maybe
import Data.List (head)

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
    (wishRegisterWidget, enctype) <- generateFormPost $ wishForm listId Nothing
    editWishForms <- generateEditWidgets listId wishes
    defaultLayout $ do
        setTitleI MsgWishListTitle
        $(widgetFile "wishlist_owner")

generateEditWidgets :: WishlistId -> [Entity Wish] -> Handler ([(Widget, Enctype)])
generateEditWidgets listId wishEntities = do
    let wishes = map (\(Entity id wish) -> Just wish) wishEntities
    let forms = map (wishForm listId) wishes
    mapM generateFormPost forms

getGuestWishList :: Wishlist -> [Entity Wish] -> Handler RepHtml
getGuestWishList listId wishes = do
  defaultLayout $ do
      setTitleI MsgWishListTitle
      $(widgetFile "wishlist_guest")

postWishListR :: WishlistId -> Handler RepHtml
postWishListR listId = do
    render <- getMessageRender
    ((result, _), _) <- runFormPost $ wishForm listId Nothing
    case result of
        FormSuccess (wish) -> do
            runDB $ insert wish
            setMessage $ toHtml $ render MsgRegisterWishWishAdded
            redirect $ (WishListR listId)
        _ -> do
            setMessage $ toHtml $ render MsgRegisterWishErrorAdding
            redirect $ (WishListR listId)

wishForm :: WishlistId -> Maybe Wish -> Form (Wish)
wishForm listId wish = renderEditWidget $ Wish
    <$> areq textField (fieldSettingsLabel MsgWishRegisterFormName) (wishName <$> wish)
    <*> areq textField (fieldSettingsLabel MsgWishRegisterFormImage) (wishImageUrl <$> wish)
    <*> areq textField (fieldSettingsLabel MsgWishRegisterFormStores) (wishStores <$> wish)
    <*> areq intField (fieldSettingsLabel MsgWishRegisterFormAmount) (wishAmount <$> wish)
    <*> pure (fromMaybe 0 (wishBought <$> wish))
    <*> pure listId

deleteWishForm :: WishId -> Form (WishId)
deleteWishForm wishId = renderBootstrap $ pure wishId

renderEditWidget :: Monad m => FormRender m a
renderEditWidget aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
    let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <td>^{fvInput view}
|]
    return (res, widget)

wishEditWidget :: WishId -> Wish -> Widget
wishEditWidget wishId wish = [whamlet|
|]
-- <tr>
--  <form method=post action=@{WishR wishId} enctype=#{enctype}>
--  <td>
