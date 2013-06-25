{-# LANGUAGE OverloadedStrings #-}
module RegisterTest
    ( registerSpecs
    ) where

import TestImport
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
            bodyContains "Copyright Ulf Lilleengen"
            bodyContains "Name of wish list"
            bodyContains "Administrator password"
            bodyContains "Guest password"

        yit "registers new lists through POST requests" $ do
            cleanDB
            get RegisterR
            request $ do
                setMethod "POST"
                setUrl RegisterR
                addNonce
                byLabel "Name of wish list" "foobar"
                byLabel "Administrator password" "foo"
                byLabel "Guest password" "bar"

            statusIs 303
            listAfter <- runDB $ selectList ([] :: [Filter Wishlist]) []
            assertEqual "wish list was not registered!" 1 $ L.length listAfter
