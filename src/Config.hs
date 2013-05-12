{-# LANGUAGE UndecidableInstances, OverlappingInstances #-}
module Config where

-- Third party.
import Data.ConfigFile
import Data.Either.Utils
import Data.List.Split

-- User configurable

data WishsysConfig = WishsysConfig {
  guestUsers :: [String],
  adminUsers :: [String],
  wishDB :: String,
  siteKey :: String,
  userDB :: String
} deriving (Show)

fromConfigFile :: String -> IO WishsysConfig
fromConfigFile fileName = do
  cf <- readfile emptyCP fileName :: IO (Either CPError ConfigParser)
  let cp = forceEither cf
  let wishdb = (forceEither $ get cp "DEFAULT" "wish_db") :: String
  let userdb = (forceEither $ get cp "DEFAULT" "user_db") :: String
  let sitekey = (forceEither $ get cp "DEFAULT" "site_key") :: String
  let guests = (forceEither $ get cp "DEFAULT" "guest_users") :: String
  let admins = (forceEither $ get cp "DEFAULT" "admin_users") :: String
  let config = WishsysConfig (splitOn "," guests)
                             (splitOn "," admins)
                             wishdb
                             sitekey
                             userdb
  putStrLn $ "String up with following config: " ++ (show config)
  return $ config
