{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Data.Maybe
import qualified Data.ByteString as B

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (serveFile "static/index.html") <|>
    route [ ("insert", insertHandler)
          , ("admin", serveFile "static/admin.html")
          ]

insertHandler :: Snap ()
insertHandler = do
    what <- getPostParam "what"
    amount <- getPostParam "amount"
    if what == Nothing
       then (writeBS "must specify 'what'")
       else if amount == Nothing
               then (writeBS "must specify 'amount'")
               else (writeBS (B.concat ["What: '", (fromJust what), "', Amount: '", (fromJust amount), "'"]))
