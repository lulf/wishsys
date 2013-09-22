{-# LANGUAGE OverloadedStrings #-}
module JsonTest
    ( jsonSpecs
    ) where

import TestImport
import Network.Wai.Test
import Data.Aeson

jsonSpecs :: Specs
jsonSpecs =
  ydescribe "The REST API" $ do
    yit "Is able to produce json prints of data" $ do
      cleanDB
      uid <- runDB $ insert $ User "ulf"  "flu"  "salt"
      lid <- runDB $ insert $ Wishlist "foolist" "foourl" uid uid
      _ <- runDB $ insert $ Wish "mywish" "myimage" "mystore" 10 1 lid
--      _ <- runDB $ insert $ Wish "barwish" "lolimg" "barstore" 12 2 lid
      get $ JsonWishListR "foourl" Guest
      statusIs 200
      resp <- getResponse
      let js = fmap (decode . simpleBody) resp :: Maybe (Maybe [Object])
      let expected = Just $ decode "[{\"amount\":10,\"image\":\"myimage\",\"name\":\"mywish\",\"remaining\":9,\"stores\":\"mystore\"}]" :: Maybe (Maybe [Object])
      assertEqual "Bad json response for guest user" expected js
