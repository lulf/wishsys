{-# LANGUAGE OverloadedStrings #-}
module JsonTest
    ( jsonSpecs
    ) where

import TestImport

jsonSpecs :: Specs
jsonSpecs =
  ydescribe "The REST API" $ do
    yit "Is able to produce json prints of data" $ do
      cleanDB
      uid <- runDB $ insert $ User "ulf"  "flu"  "salt"
      lid <- runDB $ insert $ Wishlist "foolist" "foourl" uid uid
      lid <- runDB $ insert $ Wishlist "barlist" "barurl" uid uid
      lid <- runDB $ insert $ Wishlist "bazlist" "bazurl" uid uid
      get JsonWishListListR
      statusIs 200
      printBody
      bodyEquals "[\"http://localhost/wishlist/foourl\",\"http://localhost/wishlist/barurl\",\"http://localhost/wishlist/bazurl\"]"
