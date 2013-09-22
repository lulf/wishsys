{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.JsonWishList where

import Import
import Handler.Util
import Data.Aeson
import Data.List

data SecuredWish = SecuredWish AccessLevel Wish


instance ToJSON SecuredWish where
  toJSON (SecuredWish Guest (Wish name url stores amount bought _)) =
    object ["name" .= name
           ,"image" .= url
           ,"stores" .= stores
           ,"remaining" .= (amount - bought)
           ]
  toJSON (SecuredWish Admin (Wish name url stores amount _ _)) =
    object ["name" .= name
           ,"image" .= url
           ,"stores" .= stores
           ,"amount" .= amount
           ]

getJsonWishListR :: Text -> AccessLevel -> Handler Value
getJsonWishListR listUrl accessLevel = do
  (listId, wishList) <- getWishlist listUrl accessLevel
  wishes <- getWishes listId
  let list = map (\(Entity _ wish) -> SecuredWish accessLevel wish) wishes
  return $ toJSON list
