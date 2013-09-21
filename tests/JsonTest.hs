{-# LANGUAGE OverloadedStrings #-}
module JsonTest
    ( jsonSpecs
    ) where

import TestImport

jsonSpecs :: Specs
jsonSpecs =
  ydescribe "The REST API" $ do
    yit "Is able to produce wishes as json" $ do
      statusIs 200
