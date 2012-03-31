{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import            Control.Monad
import            Control.Monad.State
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as BS
import            Data.Lens.Template
import            Data.Map ((!))
import            Data.Maybe
import            Data.String
import            Database.HDBC.Sqlite3
import            Snap
import            Snap.Core
import            Snap.Snaplet
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Auth.Backends.Hdbc
import            Snap.Snaplet.Hdbc
import            Snap.Snaplet.Session
import            Snap.Snaplet.Session.Backends.CookieSession
import            Snap.Util.FileServe
import            Data.Maybe
import qualified  Data.ByteString as B

data App = App
   { --_authLens :: Snaplet (AuthManager App)
--   , _sessLens :: Snaplet SessionManager
     _dbLens   :: Snaplet (HdbcSnaplet Connection IO)
   }

makeLenses [''App]

appInit :: SnapletInit App App
appInit = makeSnaplet "wishsys" "Wish list application" Nothing $ do
    addRoutes [ ("", serveFile "static/index.html")
              , ("wishlist", wishViewHandler)
              , ("insert", insertHandler)
              , ("register", registerHandler)
              , ("admin", serveFile "static/admin.html") ]
    let sqli = connectSqlite3 "test/test.db"
    _dblens'  <- nestSnaplet "hdbc" dbLens $ hdbcInit sqli
    return $ App _dblens'

main :: IO ()
main = serveSnaplet defaultConfig appInit

insertHandler :: Handler App App ()
insertHandler = do
    what <- getParam "what"
    amount <- getParam "amount"
    if what == Nothing
       then (writeBS "must specify 'what'")
       else if amount == Nothing
               then (writeBS "must specify 'amount'")
               else do
                    insertWish (Wish 0 (BS.unpack (fromJust what)) (read (BS.unpack (fromJust amount)) ::Integer) 0)
                    writeBS (B.concat ["Inserted: '", (fromJust what), "'. Amount: '", (fromJust amount), "'"])

registerHandler :: Handler App App ()
registerHandler = do
    wishid <- getParam "wishid"
    amount <- getParam "amount"
    if wishid == Nothing
       then (writeBS "id not given, aborting")
       else if amount == Nothing
               then (writeBS "amount not specified")
               else do registerPurchase (read (BS.unpack (fromJust wishid)) ::Integer)
                                        (read (BS.unpack (fromJust amount)) ::Integer)

registerPurchase :: Integer -> Integer -> Handler App App ()
registerPurchase wishid amount = do
    wish <- getWish wishid
    let wamount = (wishAmount wish)
    let bought = (wishBought wish)
    let remaining = wamount - bought
    if remaining - amount >= 0
       then do
            updateWish wishid (bought + amount)
            writeBS (B.concat ["Har trukket ifra ", (fromString (show amount)), " stk. av type '", (fromString (wishName wish)), "'"])
       else writeBS "Ikke nok ønsker igjen!"

formatWishEntry :: Wish -> String
formatWishEntry (Wish wishid name amount bought) =
        "<tr>" ++
        "<td>" ++ name ++ "</td>" ++
        "<td>" ++ (show remaining) ++ "</td>" ++
        "<td>" ++
        "<form action=\"register\" method=\"post\">" ++
        "<input type=\"text\" size=\"2\" name=\"amount\" value=\"0\" />" ++
        "<input type=\"hidden\" name=\"wishid\" value=\"" ++ (show wishid) ++ "\" />" ++
        "<input type=\"submit\" value=\"Registrer\" />" ++
        "</form>" ++
        "</td>" ++
        "</tr>"
    where remaining = amount - bought

wishViewHandler :: Handler App App ()
wishViewHandler = do
    wishList <- getWishes
    writeBS "<html>"
    writeBS "<h1>Ønskeliste</h1>"
    writeBS "<table border=\"1\">"
    writeBS "<tr><th>Hva</th><th>Antall</th><th>Registrer</th></tr>"
    writeBS (fromString (concat (map formatWishEntry wishList)))
    writeBS "</table>"
    writeBS "</html>"

data Wish = Wish {
    wishId     :: Integer,
    wishName   :: String,
    wishAmount :: Integer,
    wishBought :: Integer
}

getWishes :: HasHdbc m c s => m [Wish]
getWishes = do
    rows <- query "SELECT * FROM list" []
    return $ map toWish rows
    where toWish :: Row -> Wish
          toWish rw = Wish (fromSql (rw ! "id")) (fromSql (rw ! "what")) (fromSql (rw ! "amount")) (fromSql (rw ! "bought"))

getWish :: HasHdbc m c s => Integer -> m Wish
getWish wishid = do
    rows <- query "SELECT * FROM list WHERE id = ?" [toSql wishid]
    return $ head (map toWish rows)
    where toWish :: Row -> Wish
          toWish rw = Wish (fromSql (rw ! "id")) (fromSql (rw ! "what")) (fromSql (rw ! "amount")) (fromSql (rw ! "bought"))

updateWish :: HasHdbc m c s => Integer -> Integer -> m ()
updateWish wishid bought = do
    query' "UPDATE list SET bought = ? WHERE id = ?" [toSql bought, toSql wishid]
    return ()

insertWish:: HasHdbc m c s => Wish -> m ()
insertWish (Wish _ name amount _ ) = do
    let sqlList = [toSql name, toSql amount]
    query' "INSERT INTO list (what, amount, bought) VALUES(?, ?, 0)" sqlList
    return ()

instance HasHdbc (Handler App App) Connection IO where
    getHdbcState = with dbLens get
