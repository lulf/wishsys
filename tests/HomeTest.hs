{-# LANGUAGE OverloadedStrings #-}
module HomeTest
    ( homeSpecs
    ) where

import TestImport
import qualified Data.List as L
import           Text.Shakespeare.I18N         (RenderMessage (..))
import qualified Data.Text as T


getMessageRender = do
    y <- getTestYesod
    return $ renderMessage y ["en"]

homeSpecs :: Specs
homeSpecs =
    ydescribe "The home page" $ do

        yit "displays the correct elements" $ do
            render <- getMessageRender
            get HomeR
            statusIs 200
            htmlAnyContain "h1" $ T.unpack $ render MsgHomeHeader
            bodyContains $ T.unpack $ render MsgHomeWelcome
            bodyContains "Lilleengen Programvarefabrikk"
