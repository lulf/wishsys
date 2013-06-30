{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.WishList where

import Import
import Yesod.Auth
import Data.Maybe

getWishes :: WishlistId -> Handler ([Entity Wish])
getWishes listId = runDB $ selectList ([WishWlist ==. listId] :: [Filter Wish]) [Asc WishName]

getWishListR :: WishlistId -> AccessLevel -> Handler Html
getWishListR listId _ = do
    maybeList <- runDB $ get listId
    userId <- requireAuthId
    wishes <- getWishes listId
    case maybeList of
        Just (Wishlist _ ownerId guestId) ->
          if userId == guestId
          then getGuestWishList listId wishes
          else if userId == ownerId
               then getOwnerWishList listId wishes
               else redirect HomeR
        _ -> redirect HomeR

getOwnerWishList :: WishlistId -> [Entity Wish] -> Handler Html
getOwnerWishList listId wishes = do
    (wishRegisterWidget, enctype) <- generateFormPost $ wishOwnerForm listId Nothing
    editWishForms <- generateEditWidgets listId wishes
    defaultLayout $ do
        setTitleI MsgWishListTitle
        $(widgetFile "wishlist_owner")

generateEditWidgets :: WishlistId -> [Entity Wish] -> Handler ([(WishId, (Widget, Enctype), (Widget, Enctype))])
generateEditWidgets listId wishEntities = do
    let wishes = map (\(Entity _ wish) -> Just wish) wishEntities
    let wishIds = map (\(Entity eid _) -> eid) wishEntities
    let forms = map (wishOwnerForm listId) wishes
    let deleteForms = map deleteWishForm wishIds
    deletePosts <- mapM generateFormPost deleteForms
    posts <- mapM generateFormPost forms
    return $ zip3 wishIds posts deletePosts

getGuestWishList :: WishlistId -> [Entity Wish] -> Handler Html
getGuestWishList listId wishes = do
  guestForms <- generateGuestForms wishes
  defaultLayout $ do
      setTitleI MsgWishListTitle
      $(widgetFile "wishlist_guest")

generateGuestForms :: [Entity Wish] -> Handler ([(Widget, Enctype)])
generateGuestForms wishEntities = do
    let forms = map (\_ -> wishGuestForm) wishEntities
    mapM generateFormPost forms

wishGuestForm :: Form (Int)
wishGuestForm = renderBootstrap $ areq intField "" (Just 0)

postWishListR :: WishlistId -> AccessLevel -> Handler Html
postWishListR listId accessLevel = do
    render <- getMessageRender
    ((result, _), _) <- runFormPost $ wishOwnerForm listId Nothing
    case result of
        FormSuccess (wish) -> do
            _ <- runDB $ insert wish
            setMessage $ toHtml $ render MsgRegisterWishWishAdded
            redirect $ (WishListR listId accessLevel)
        _ -> do
            setMessage $ toHtml $ render MsgRegisterWishErrorAdding
            redirect $ (WishListR listId accessLevel)

wishOwnerForm :: WishlistId -> Maybe Wish -> Form (Wish)
wishOwnerForm listId wish = renderEditWidget $ Wish
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
