{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Auth where

import           Config
import           Data.ByteString.Char8       (ByteString)
import qualified Data.Text
import           Render
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist          as H
import           Text.Blaze.Renderer.XmlHtml

--------------------
-- Authentication --
--------------------

-- Add routes that are authenticated by a user. Essentially wraps handlers in
-- the handleAsUser function.
addAuthRoutes :: [(ByteString, Handler App App (), [String])] -> Initializer App App ()
addAuthRoutes routeList = do
    let authRouteList = map createAuthRoute routeList
    addRoutes authRouteList

createAuthRoute :: (ByteString, Handler App App (), [String]) -> (ByteString, Handler App App ())
createAuthRoute (routePath, handler, []) = (routePath, handler)
createAuthRoute (routePath, handler, userList) = (routePath, handleAsUser userList handler)

-- Performs the actual login.
loginHandler :: Handler App App ()
loginHandler = with authLens $ do
    loginUser "login" "password" (Just "remember") onFailure onSuccess
    where onFailure _ = loginForm True
          onSuccess   = do
                        mu <- currentUser
                        case mu of
                                Just user -> dispatchUser user
                                Nothing -> loginForm True



loginForm :: Bool -> Handler App (AuthManager App) ()
loginForm True = do
    renderWithSplices "login" [("notification", loginFailedNotification)]
loginForm False = H.render "login"

loginFailedNotification :: SnapletSplice App (AuthManager App)
loginFailedNotification = return . renderHtmlNodes $ insertNotification "Kunne ikke logge inn: brukernavn/passord er ugyldig"

isAdmin :: String -> Bool
isAdmin user = user `elem` adminUsers

isGuest:: String -> Bool
isGuest user = user `elem` guestUsers

dispatchUser :: AuthUser -> Handler App (AuthManager App) ()
dispatchUser authUser = do
    if isGuest user
       then redirect "/wishlist"
       else if isAdmin user
               then redirect "/admin"
               else redirect "/"
  where user = Data.Text.unpack (userLogin authUser)

-- Verifies user credentials and username before running handler
handleAsUser :: [String] -> (Handler App App ()) -> Handler App App ()
handleAsUser userList fn = do
    mu <- with authLens currentUser
    case mu of
      Just u -> let userName = Data.Text.unpack (userLogin u)
                in if userName `elem` userList
                      then fn
                      else redirect "/"
      Nothing -> redirect "/"

-- Redirect to a value if set
redirectTo :: MonadSnap m => Maybe ByteString -> m b
redirectTo dest = do
    case dest of
              Nothing -> redirect "/"
              Just uri -> redirect uri

-- Log out a user, redirect to main page.
logoutHandler :: Handler App App ()
logoutHandler = do
  with authLens logout
  redirect "/"
