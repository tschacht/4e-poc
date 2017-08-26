{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Control.Monad.Trans          (lift, liftIO)
import Control.Monad.Trans.Reader   (ask)
import Data.Pool                    (withResource)
import Data.Text.Lazy as DTL        (Text, toStrict, fromStrict)
import Data.Text as DT              (append, pack)
import Database.Bolt
import Web.Scotty.Trans             (ActionT, file, param, json, rescue, text)

import Type
import Data

-- |Run BOLT action in scotty 'ActionT' monad tansformer
runQ :: BoltActionT IO a -> ActionT Text WebM a
runQ act = do ss <- lift ask
              liftIO $ withResource (pool ss) (`run` act)

-- |Main page route
mainR :: ActionT Text WebM ()
mainR = file "index.html"

-- |Graph response route
graphR :: ActionT Text WebM ()
graphR = do limit <- param "limit" `rescue` const (return 100)
            graph <- runQ $ queryGraph limit
            json graph

-- |Search response route
searchR :: ActionT Text WebM ()
searchR = do q <- param "q" :: ActionT Text WebM Text
             results <- runQ $ querySearch $ toStrict q
             json results

-- |Movie response route
movieR :: ActionT Text WebM ()
movieR = do t <- param "title" :: ActionT Text WebM Text
            movieInfo <- runQ $ queryMovie $ toStrict t
            json movieInfo

-- | *** Demo stuff ***

demoInfoR :: ActionT Text WebM ()
demoInfoR = do
  date <- liftIO queryDemoDate
  fileData <- liftIO queryDemoFile
  (pingExit, pingOut, pingErr) <- liftIO queryDemoPing
  json $ DemoResult date fileData (DemoShellResult pingExit pingOut pingErr)

demoZipR :: ActionT Text WebM ()
demoZipR = do
  liftIO createDemoZip
  file "./result.zip"

demoWebdriverR :: ActionT Text WebM ()
demoWebdriverR = do
  liftIO runDemoWebdriver
  file "./screenshot.png"

demoRestReqR :: ActionT Text WebM ()
demoRestReqR = do
  r <- liftIO queryDemoRestApi
  -- persist result
  runQ $ persistDemoNode $ nodeText r
  --return again as json (i.e. equal to demoInfoR result)
  json r
  where
    getdate (DemoResult date _ _) = date
    getpingout (DemoResult _ _ (DemoShellResult _ pingOut _)) = pingOut
    nodeText r = getdate r `append` DT.pack ": " `append` getpingout r
