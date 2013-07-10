{-# LANGUAGE OverloadedStrings #-}
module RegisterTest
    ( registerSpecs
    ) where

import TestImport
import qualified Data.Text as T
import qualified Data.List as L

cleanDB :: YesodExample App ()
cleanDB = do
    runDB $ deleteWhere ([] :: [Filter Wish])
    runDB $ deleteWhere ([] :: [Filter Wishlist])
    runDB $ deleteWhere ([] :: [Filter User])

registerSpecs :: Specs
registerSpecs =
    ydescribe "The register page" $ do

        yit "displays the correct elements" $ do
            cleanDB
            get RegisterR
            statusIs 200
            htmlAllContain "h1" "Create new wish list"
            bodyContains "Lilleengen Programvarefabrikk"
            bodyContains "Name of wish list"
            bodyContains "Administrator password"
            bodyContains "Guest password"

        yit "registers new lists and users through POST requests" $ do
            cleanDB
            get RegisterR
            request $ do
                setMethod "POST"
                setUrl RegisterR
                addNonce
                byLabel "Name of wish list" "foo bar LOLZ"
                byLabel "Short name used for guest URL (http://wishsys.dimling.net/wishlist/<short name>/guest)" "foobar"
                byLabel "Administrator password" "foo"
                byLabel "Guest password" "bar"

            statusIs 303
            lists <- runDB $ selectList ([] :: [Filter Wishlist]) []
            assertEqual "wish list was not registered!" 1 $ L.length lists
            let (Entity _ list) = head lists
            assertEqual "list name is not correct" "foobar" $ wishlistUrlName list

            users <- runDB $ selectList ([] :: [Filter User]) []
            assertEqual "users not registered" 2 $ L.length users
            let (Entity _ first) = head users
            let (Entity _ second) = head $ tail users
            assertEqual "admin user not correct" "admin_foobar" $ T.unpack $ userName first
            assertEqual "guest user not correct" "guest_foobar" $ T.unpack $ userName second

        yit "provides a useful error message if list already exists" $ do
            cleanDB
            get RegisterR
            request $ do
                setMethod "POST"
                setUrl RegisterR
                addNonce
                byLabel "Name of wish list" "foo bar LOLZ"
                byLabel "Short name used for guest URL (http://wishsys.dimling.net/wishlist/<short name>/guest)" "foobar"
                byLabel "Administrator password" "foo"
                byLabel "Guest password" "bar"
            statusIs 303

            get RegisterR
            request $ do
                setMethod "POST"
                setUrl RegisterR
                addNonce
                byLabel "Name of wish list" "foo bar LOLZ"
                byLabel "Short name used for guest URL (http://wishsys.dimling.net/wishlist/<short name>/guest)" "foobar"
                byLabel "Administrator password" "mamma"
                byLabel "Guest password" "pappa"
            statusIs 303
            get RegisterR
            htmlAnyContain "div" "Wish list already exists, please choose a different name"

