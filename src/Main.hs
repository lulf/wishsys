main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS (readFile "static/index.html")) <|>
    route [ ("insert", writeBS "Inserting wish")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

