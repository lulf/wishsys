{-# LANGUAGE OverloadedStrings #-}
module HomeTest
    ( homeSpecs
    ) where

import TestImport
import qualified Data.List as L
import           Data.Text

homeSpecs :: Specs
homeSpecs =
    ydescribe "The home page" $ do

        yit "displays the correct elements" $ do
            render <- getMessageRender
            get HomeR
            statusIs 200
            htmlAnyContain "h1" $ unpack $ render MsgHomeHeader
            bodyContains $ unpack $ render MsgHomeWelcome
            bodyContains "Lilleengen Programvarefabrikk"
