{-# LANGUAGE OverloadedStrings #-}
module WishListGuestTest
    ( wishListGuestSpecs
    ) where

import TestImport
import qualified Data.Text as T
import qualified Data.List as L

cleanDB :: YesodExample App ()
cleanDB = do
    runDB $ deleteWhere ([] :: [Filter Wish])
    runDB $ deleteWhere ([] :: [Filter Wishlist])
    runDB $ deleteWhere ([] :: [Filter User])

wishListGuestSpecs :: Specs
wishListGuestSpecs =
    ydescribe "The wish list guest page" $ do

        yit "correctly updates the number of available purchases" $ do
            cleanDB
            get RegisterR
            request $ do
                setMethod "POST"
                setUrl RegisterR
                addNonce
                byLabel "Name of wish list" "test list"
                byLabel "Short name used for guest URL (http://wishsys.lulf.no/wishlist/<short name>/guest)" "foobar"
                byLabel "Administrator password" "foo"
                byLabel "Guest password" "bar"
            statusIs 303
            get $ WishListR "foobar" Admin
            bodyContains "Lilleengen Programvarefabrikk"

