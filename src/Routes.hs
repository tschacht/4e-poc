{-# LANGUAGE OverloadedStrings #-}

module Routes where

import           Control.Monad.Trans        (lift, liftIO)
import           Control.Monad.Trans.Reader (ask)
import           Data.Pool                  (withResource)
import           Data.Text as DT            (Text)
import           Data.Text.Lazy as DTL      (Text, toStrict)
import           Database.Bolt
import           Web.Scotty.Trans           (ActionT, file, param, json, rescue, text)

import           Type
import           Data

-- |Run BOLT action in scotty 'ActionT' monad tansformer
runQ :: BoltActionT IO a -> ActionT DTL.Text WebM a
runQ act = do ss <- lift ask
              liftIO $ withResource (pool ss) (`run` act)

-- |Main page route
mainR :: ActionT DTL.Text WebM ()
mainR = file "index.html"

-- |Graph response route
graphR :: ActionT DTL.Text WebM ()
graphR = do limit <- param "limit" `rescue` const (return 100)
            graph <- runQ $ queryGraph limit
            json graph

-- |Search response route
searchR :: ActionT DTL.Text WebM ()
searchR = do q <- param "q" :: ActionT DTL.Text WebM DTL.Text
             results <- runQ $ querySearch (toStrict q)
             json results

-- |Movie response route
movieR :: ActionT DTL.Text WebM ()
movieR = do t <- param "title" :: ActionT DTL.Text WebM DTL.Text
            movieInfo <- runQ $ queryMovie (toStrict t)
            json movieInfo

-- | *** Demo stuff ***

demoInfoR :: ActionT DTL.Text WebM ()
demoInfoR = do
  rDate <- liftIO queryDemoDate
  rFile <- liftIO queryDemoFile
  (rPingC, rPingO, rPingE) <- liftIO queryDemoPing
  json $ DemoResult rDate rFile (DemoShellResult rPingC rPingO rPingE)
  {-- win
  let nope = "not supported on win"
  json $ DemoResult nope rFile (DemoShellResult nope nope nope)
  -}

demoZipR :: ActionT DTL.Text WebM ()
demoZipR = do
  liftIO createDemoZip
  file "./result.zip"

demoWebdriverR :: ActionT DTL.Text WebM ()
demoWebdriverR = do
  liftIO runDemoWebdriver
  file "./screenshot.png"

demoRestReq :: ActionT DTL.Text WebM ()
demoRestReq = do
  r <- liftIO queryDemoRestApi
  text r
