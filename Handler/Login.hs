{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Login where

import Yesod.Auth
import Yesod.Auth.Message
import Yesod.Auth.HashDB (validateUser)
import Import
import Data.Text (pack, unpack)
import Data.Maybe

getLoginName :: AccessLevel -> Text -> Text
getLoginName accessLevel name =
  case accessLevel of
    Admin -> pack $ "admin_" ++ (unpack name)
    Guest -> pack $ "guest_" ++ (unpack name)

doLogin :: Text -> Text -> Handler ()
doLogin mu mp = do
    let uid = Just $ UniqueUser mu 
    isValid <- fromMaybe (return False) (validateUser <$> uid <*> (Just mp))
    if isValid 
       then setCreds False $ Creds "hashdb" mu []
       else do
            render <- getMessageRender
            loginErrorMessage (HomeR) (render MsgInvalidUserOrPassword)
