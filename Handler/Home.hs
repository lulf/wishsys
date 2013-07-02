{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Handler.Login

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitleI MsgHomeTitle
        $(widgetFile "homepage")
