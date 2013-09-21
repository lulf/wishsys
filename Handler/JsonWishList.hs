{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.JsonWishList where

import Import

foo :: Text
foo = "foo"

bar :: Text
bar = "bar"

getJsonWishListR :: Text -> AccessLevel -> Handler Value
getJsonWishListR _ _ = do
  return $ object [ foo .= bar ]
