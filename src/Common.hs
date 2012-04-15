{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common where

-- Third party.
import            Data.String

type WishID = Integer

-- Wish data type
data Wish = Wish {
    wishId     :: WishID,
    wishName   :: String,
    wishImg    :: String,
    wishStore  :: String,
    wishAmount :: Integer,
    wishBought :: Integer
}
