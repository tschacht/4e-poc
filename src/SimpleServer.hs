{-# LANGUAGE OverloadedStrings #-}

module SimpleServer
    ( runServer
    ) where

import Control.Monad.Trans.Reader             (runReaderT)
import Data.Text.Lazy                         (Text)
import Network.Wai.Middleware.RequestLogger   (logStdoutDev)
import Web.Scotty.Trans                       (ScottyT, defaultHandler, get, middleware, scottyT)
import Database.Bolt                          (BoltCfg)
import Routes
import Data

type Port = Int

-- |Run server with connection pool as a state
runServer :: Port -> BoltCfg -> IO ()
runServer port config = do state <- constructState config
                           scottyT port (`runReaderT` state) $ do
                             middleware logStdoutDev
                             get "/"                  mainR
                             get "/graph"             graphR
                             get "/search"            searchR
                             get "/movie/:title"      movieR
                             get "/demo-cl"           demoInfoR
                             get "/demo-cl-zip"       demoZipR
                             get "/demo-cl-sc"        demoWebdriverR
                             get "/demo-ctrl-rest"    demoRestReqR
