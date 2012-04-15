{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- This module contains all rendering functions
module Render (
    insertNotification,
    imgUrl,
    wishEditFormTable,
    wishTable
) where

-- Third party.
import qualified  Text.Blaze.Html5 as HTML
import qualified  Text.Blaze.Html5.Attributes as ATTR
import            Common

-- Creates a insert notification HTML tag.
insertNotification :: String -> HTML.Html
insertNotification msg = HTML.div HTML.! ATTR.id "notification" $ HTML.p $ HTML.toHtml msg

-- Creates an image url html tag from a string
imgUrl :: String -> HTML.Html
imgUrl url = HTML.a HTML.! ATTR.href (HTML.toValue url) $ HTML.img HTML.!  ATTR.src (HTML.toValue url) HTML.!  ATTR.width "100" HTML.!  ATTR.height "100"

-- Creates a form input entry given the input name, value and type
editFormEntry :: String -> String -> String -> HTML.Html
editFormEntry name value attrType =
    HTML.input HTML.! ATTR.type_ (HTML.toValue attrType) HTML.! ATTR.name (HTML.toValue name) HTML.! ATTR.value (HTML.toValue value)

-- Creates an edit form of a wish, which can be submitted
wishEditForm :: Wish -> HTML.Html
wishEditForm (Wish wishid name url store amount _ ) = do
    HTML.form HTML.! ATTR.action "/admin/edit" HTML.! ATTR.method "post" HTML.!  ATTR.acceptCharset "ISO8859-1" $ do
        HTML.tr $ do
            editFormEntry "wishId" (show wishid) "hidden"
            HTML.td $ editFormEntry "wishName" name "text"
            HTML.td $ do editFormEntry "wishUrl" url "text" 
                         imgUrl url
            HTML.td $ editFormEntry "wishStore" store "text"
            HTML.td $ editFormEntry "wishAmount" (show amount) "text"
            HTML.td $ editFormEntry "wishDeleteFlag" "delete" "checkbox"
            HTML.td $ HTML.input HTML.! ATTR.type_ "submit" HTML.! ATTR.value "Oppdater"

-- Converts a list of wishes to a editable form.
wishEditFormTable :: [Wish] -> HTML.Html
wishEditFormTable wishList = HTML.toHtml $ map wishEditForm wishList

wishTable :: [Wish] -> HTML.Html
wishTable wishList = HTML.toHtml $ map wishTableEntry wishList

-- Creates a table row from a wish for wish list display
wishTableEntry :: Wish -> HTML.Html
wishTableEntry wish = do
    HTML.tr $ do
              HTML.td $ HTML.toHtml name
              HTML.td $ imgUrl url
              HTML.td $ HTML.toHtml store
              HTML.td $ HTML.toHtml remaining
              HTML.td $ registrationForm wishid
  where name      = wishName wish
        url       = wishImg wish
        store     = wishStore wish
        remaining = (wishAmount wish) - (wishBought wish)
        wishid    = (wishId wish)

-- Create the registration form for a wish given a wish id
registrationForm :: WishID -> HTML.Html
registrationForm wishid =
    HTML.form HTML.! ATTR.action "/wishlist/insert" HTML.!  ATTR.method "post" $ do
              HTML.input HTML.! ATTR.type_ "text" HTML.! ATTR.size "2" HTML.!  ATTR.name "amount" HTML.! ATTR.value "0"
              HTML.input HTML.! ATTR.type_ "hidden" HTML.! ATTR.name "wishid" HTML.! ATTR.value (HTML.toValue wishid)
              HTML.input HTML.! ATTR.type_ "submit" HTML.! ATTR.value "Registrer"
