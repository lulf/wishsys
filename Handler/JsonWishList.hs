{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.JsonWishList where

import Import
import Handler.Util
import Data.Aeson
import Data.List

instance ToJSON Wish where
  toJSON (Wish name url stores amount bought _) = object ["name" .= name ]

getJsonWishListR :: Text -> AccessLevel -> Handler Value
getJsonWishListR listUrl accessLevel = do
  (listId, wishList) <- getWishlist listUrl accessLevel
  wishes <- getWishes listId
  let list = map (\(Entity _ wish) -> wish) wishes
  return $ toJSON list
