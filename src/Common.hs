{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common where

-- Third party.
import            Snap.Snaplet.Heist as H
import qualified  Text.Blaze.Html5 as HTML
import qualified  Text.Blaze.Html5.Attributes as ATTR
import            Text.Blaze.Renderer.XmlHtml
import            Data.String

insertNotification :: String -> HTML.Html
insertNotification msg = HTML.div HTML.! ATTR.id "notification" $ HTML.p $ HTML.toHtml msg

type WishID = Integer

-- Wish data type
data Wish = Wish {
    wishId     :: WishID,
    wishName   :: String,
    wishImg    :: String,
    wishStore  :: String,
    wishAmount :: Integer,
    wishBought :: Integer
}
