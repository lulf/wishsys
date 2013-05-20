{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Persistence where

import           Common
import           Data.Map          ((!))
import           Snap.Snaplet.Hdbc
---------------------------------------------
-- Functions for interacting with database --
---------------------------------------------

-- Get a list of all wishes
getWishes :: HasHdbc m c s => m [Wish]
getWishes = do
    rows <- query "SELECT * FROM list" []
    return $ map constructWish rows

-- Constructs a wish from a database row
constructWish :: Row -> Wish
constructWish row = Wish (fromSql (row ! "id"))
                         (fromSql (row ! "what"))
                         (fromSql (row ! "url"))
                         (fromSql (row ! "store"))
                         (fromSql (row ! "amount"))
                         (fromSql (row ! "bought"))

-- Get a specific wish given an id
getWish :: HasHdbc m c s => Integer -> m Wish
getWish wishid = do
    rows <- query "SELECT * FROM list WHERE id = ?" [toSql wishid]
    return $ head (map constructWish rows)

-- Update a wish (given its id) with a new value for the number of bought items
updateWishBought :: HasHdbc m c s => Integer -> Integer -> m ()
updateWishBought wishid bought = do
    query' "UPDATE list SET bought = ? WHERE id = ?" [toSql bought, toSql wishid]
    return ()

updateWish :: HasHdbc m c s => Wish -> m ()
updateWish (Wish wishid name url store amount _ ) = do
    let sqlList = [toSql name, toSql url, toSql store, toSql amount, toSql wishid]
    query' "UPDATE list SET what = ?, url = ?, store = ?, amount = ? WHERE id = ?" sqlList
    return ()

-- Insert a new wish entity into the database. The id and bought parameters to
-- the wish are ignored
insertWish:: HasHdbc m c s => Wish -> m ()
insertWish (Wish _ name url store amount _ ) = do
    let sqlList = [toSql name, toSql url, toSql store, toSql amount]
    query' "INSERT INTO list (what, url, store, amount, bought) VALUES(?, ?, ?, ?, 0)" sqlList
    return ()


-- Delete a wish (given its id) from the database.
deleteWish :: HasHdbc m c s => Integer -> m ()
deleteWish wishid = do
    query' "DELETE FROM list WHERE id = ?" [toSql wishid]
    return ()
