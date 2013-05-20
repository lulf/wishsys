module Model where

type WishId = Integer

-- Wish data type
data Wish = Wish {
    name :: String,
    imageUrl :: String,
    stores :: [String],
    amount :: Integer,
    bought :: Integer
}
