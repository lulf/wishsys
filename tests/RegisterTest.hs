{-# LANGUAGE OverloadedStrings #-}
module RegisterTest
    ( registerSpecs
    ) where

import TestImport
import qualified Data.List as L

registerSpecs :: Specs
registerSpecs =
    ydescribe "The register page" $ do

        yit "displays the correct elements" $ do
            get RegisterR
            statusIs 200
            printBody
            htmlAllContain "h1" "Create new wish list"
            bodyContains "Copyright Ulf Lilleengen"
            bodyContains "Name of wish list"
            bodyContains "Administrator password"
            bodyContains "Guest password"

        yit "registers new lists through POST requests" $ do
            get RegisterR
            statusIs 200
