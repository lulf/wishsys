{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.JsonWishListList where

import Import

foo :: Text
foo = "foo"

bar :: Text
bar = "bar"

getJsonWishListListR :: Handler Value
getJsonWishListListR = do
  return $ object [ foo .= bar ]
