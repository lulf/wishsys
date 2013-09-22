{-# LANGUAGE OverloadedStrings #-}
module JsonTest
    ( jsonSpecs
    ) where

import TestImport
import Network.Wai.Test
import Data.Aeson
import Data.ByteString.Lazy

getJsonResponseBody :: YesodExample App (Maybe (Maybe [Object]))
getJsonResponseBody = do
  resp <- getResponse
  return $ fmap (decode . simpleBody) resp

expectedJsonData :: ByteString -> Maybe (Maybe [Object])
expectedJsonData val = Just $ decode val

assertJsonResponse :: String -> ByteString -> YesodExample App ()
assertJsonResponse errorMsg expected = do
      js <- getJsonResponseBody
      let expectedJson = expectedJsonData expected
      assertEqual errorMsg expectedJson js

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
      assertJsonResponse "Bad json response for guest user" "[{\"amount\":10,\"image\":\"myimage\",\"name\":\"mywish\",\"remaining\":9,\"stores\":\"mystore\"}]"

      get $ JsonWishListR "foourl" Admin
      statusIs 200
      printBody
      assertJsonResponse "Bad json response for admin user" "[{\"amount\":10,\"image\":\"myimage\",\"name\":\"mywish\",\",\"stores\":\"mystore\"}]"
