module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Data.Text
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import System.Log.FastLogger (Logger)
import Yesod.Auth.HashDB (authHashDB, getAuthIdHashDB)

data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        maid <- maybeAuthId
        muser <- case maid of
                    Nothing -> return $ Nothing
                    Just aid -> runDB $ get aid
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_css
                ])
            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just HomeR

    isAuthorized HomeR _ = return Authorized

    isAuthorized (WishListR listUrl Admin) _ = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> redirect $ WishListLoginR listUrl Admin
            Just (Entity userid _) -> runDB $ do
                                        maybeUser <- get userid
                                        let un = case maybeUser of
                                                     Nothing -> "anonymous"
                                                     Just user -> userName user
                                        wishList <- selectList ([WishlistUrlName ==.  listUrl, WishlistOwner ==. userid]) []
                                        case wishList of
                                            [] -> return $ Unauthorized $ append "You do not have permission to view this wish list. Currently logged in as " un
                                            _ -> return Authorized

    isAuthorized (WishListR listUrl Guest) _ = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> redirect $ WishListLoginR listUrl Guest
            Just (Entity userid _) -> runDB $ do
                                        wishList <- selectList ([WishlistUrlName ==.  listUrl, WishlistGuest ==. userid]) []
                                        case wishList of
                                            [] -> return $ Unauthorized "You do not have permission to view this wish list"
                                            _ -> return Authorized

    isAuthorized (WishHandlerR listUrl Admin _) _ = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just (Entity userid _) -> runDB $ do
                                        wishList <- selectList ([WishlistUrlName ==.  listUrl, WishlistOwner ==. userid]) []
                                        case wishList of
                                            [] -> return $ Unauthorized "You do not have permission to view this wish list"
                                            _ -> return Authorized

    isAuthorized (WishHandlerR listUrl Guest _) _ = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just (Entity userid _) -> runDB $ do
                                        wishList <- selectList ([WishlistUrlName ==.  listUrl, WishlistGuest ==. userid]) []
                                        case wishList of
                                            [] -> return $ Unauthorized "You do not have permission to view this wish list"
                                            _ -> return Authorized

    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = getAuthIdHashDB AuthR (Just . UniqueUser) creds

    authPlugins _ = [authHashDB (Just . UniqueUser)]

    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ [] = defaultFormMessage
    renderMessage _ ("no":_ ) = defaultFormMessage
    renderMessage _ ("en":_ ) = defaultFormMessage
    renderMessage m (_   :ls) = renderMessage m ls

renderMsg = renderMessage App ["en", "no"]

-- For access levels data type
instance RenderMessage App AccessLevel where
    renderMessage _ [] = renderEnglish
    renderMessage _ ("no":_) = renderNorwegian
    renderMessage _ ("en":_) = renderEnglish
    renderMessage m (_   :ls) = renderMessage m ls

renderEnglish :: AccessLevel -> Text
renderEnglish Guest = "Guest"
renderEnglish Admin = "Administrator"

renderNorwegian :: AccessLevel -> Text
renderNorwegian Guest = "Gjest"
renderNorwegian Admin = "Administrator"

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod
