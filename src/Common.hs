{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common where

-- Third party.
import            Control.Monad.State
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as BS (concat, pack, unpack)
import            Data.Lens.Template
import            Data.Map ((!))
import qualified  Data.Text
import            Database.HDBC.Sqlite3
import            Snap
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Heist as H
import            Snap.Snaplet.Auth.Backends.JsonFile
import            Snap.Snaplet.Hdbc
import            Snap.Snaplet.Session
import            Snap.Snaplet.Session.Backends.CookieSession
import            Snap.Util.FileServe
import qualified  Text.Blaze.Html5 as HTML
import qualified  Text.Blaze.Html5.Attributes as ATTR
import            Text.Blaze.Renderer.XmlHtml

insertNotification :: String -> HTML.Html
insertNotification msg = HTML.div HTML.! ATTR.id "notification" $ HTML.p $ HTML.toHtml msg

-- Wish data type
data Wish = Wish {
    wishId     :: Integer,
    wishName   :: String,
    wishImg    :: String,
    wishStore  :: String,
    wishAmount :: Integer,
    wishBought :: Integer
}
