{-# LANGUAGE OverloadedStrings #-}
module HomeTest
    ( homeSpecs
    ) where

import TestImport
import qualified Data.List as L

homeSpecs :: Specs
homeSpecs =
    ydescribe "The home page" $ do

        yit "displays the correct elements" $ do
            get HomeR
            statusIs 200
            htmlAnyContain "h1" "Wishsys"
            bodyContains "Welcome to wishsys"
            bodyContains "Lilleengen Programvarefabrikk"
