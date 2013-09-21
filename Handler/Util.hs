{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Util (getWishes, getWishlist) where

import Import
import Data.Maybe

-- TODO: Merge into one database call?
getWishes :: WishlistId -> Handler ([Entity Wish])
getWishes listId = runDB $ selectList ([WishWlist ==. listId] :: [Filter Wish]) [Asc WishName]

getWishlist :: Text -> AccessLevel -> Handler (WishlistId, Wishlist)
getWishlist urlListName accessLevel = do
    render <- getMessageRender
    wl <- runDB $ selectList [WishlistUrlName ==. urlListName] [LimitTo 1]
    case wl of
        [] -> do
            setMessage $ toHtml $ render MsgWishListNotFound
            redirect $ WishListLoginR urlListName accessLevel
        (Entity wid wlist):_ -> do
            return $ (wid, wlist)
