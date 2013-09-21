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
      _ <- runDB $ insert $ Wish "mywish" "myimage" "mystore" 10 1 lid
      _ <- runDB $ insert $ Wish "barwish" "lolimg" "barstore" 12 2 lid
      get $ JsonWishListR "foourl" Guest
      statusIs 200
      printBody
      bodyEquals "[\"http://localhost/wishlist/foourl\",\"http://localhost/wishlist/barurl\",\"http://localhost/wishlist/bazurl\"]"
