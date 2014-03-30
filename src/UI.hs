{-# LANGUAGE OverloadedStrings #-}

module UI ( uiMain ) where

import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Concurrent.STM   (atomically, isEmptyTChan, newTChan,
                                           newTMVar, putTMVar, readTChan,
                                           readTMVar, takeTMVar)
import           Control.Monad            (liftM, when)
import           Control.Monad.Loops      (whileM_)
import qualified Data.Text                as T
import           Graphics.Vty
import           Graphics.Vty.Widgets.All
import           System.Directory         (doesDirectoryExist)
import           System.Exit              (exitFailure, exitSuccess)

import           FS                       (collect, fuzzyMatch, socketPath)

data UI = UI { uiList       :: Widget (List T.Text FormattedText)
             , uiProgress   :: Widget FormattedText
             , uiInput      :: Widget Edit
             , uiCollection :: Collection }

liAttr :: Attr
liAttr = black `on` white

inputAttr :: Attr
inputAttr = white `on` black

initialUI :: IO UI
initialUI = do
  lw <- newTextList liAttr []
        >>= withFocusAttribute liAttr
  pw <- textWidget wrap "0/0"
  iw <- editWidget
        >>= withFocusAttribute inputAttr
        >>= withNormalAttribute inputAttr
  c  <- newCollection
  return UI { uiList       = lw
            , uiProgress   = pw
            , uiInput      = iw
            , uiCollection = c  }

printSelection :: UI -> IO ()
printSelection st = do
  selected <- getSelected (uiList st)
  case selected of
    Nothing          -> exitFailure
    Just (_, (t, _)) -> do
      let f = T.unpack t
      isDir  <- doesDirectoryExist f
      socket <- socketPath
      writeFile socket $ (if isDir
                          then "cd '"
                          else "$EDITOR '") ++ f ++ "'\n"
      exitSuccess

uiMain :: IO ()
uiMain = do
  cc <- atomically newTChan
  cs <- atomically $ newTMVar []

  st <- initialUI
  ui <- centered =<<      return (uiList st)
                     <--> return (uiProgress st)
                     <--> return (uiInput st)

  fg <- newFocusGroup
  _  <- addToCollection (uiCollection st) ui fg
  _  <- addToFocusGroup fg (uiInput st)
  _  <- addToFocusGroup fg (uiList st)

  let updateList s = do mli <- getSelected (uiList st)
                        _   <- clearList (uiList st)
                        xs  <- atomically (readTMVar cs)
                        let fs = filter (fuzzyMatch $ T.unpack s) xs
                            pg = show (length fs) ++ "/" ++ show (length xs)
                        setText (uiProgress st) $ T.pack pg
                        mapM_ appendItem $ take 100 fs
                        case mli of
                          Nothing     -> return ()
                          Just (i, _) -> do
                            size <- getListSize (uiList st)
                            when (size > i) $ setSelected (uiList st) i

      refreshList = do t <- getEditText (uiInput st)
                       _ <- atomically $ do xs  <- readTChan cc
                                            xs' <- takeTMVar cs
                                            putTMVar cs $ xs' ++ xs
                       _ <- forkIO $ schedule $ updateList t
                       return ()

      appendItem s = let text = T.pack s
                      in plainText text >>= addToList (uiList st) text

      handleGlobal _ k ms | k == KEnter     = activateCurrentItem (uiList st) >> return False
                          | MCtrl `elem` ms = handleCtrl k >> return False
                          | otherwise       = return False

      handleCtrl k | k == KASCII 'c' = exitFailure
                   | k == KASCII 'p' = scrollUp (uiList st)
                   | k == KASCII 'n' = scrollDown (uiList st)
                   | k == KASCII 'w' = setEditText (uiInput st) T.empty
                   | otherwise       = return ()

  _ <- forkIO $ collect cc "."
  _ <- forkIO $ do
    threadDelay 250000
    whileM_ (liftM not $ atomically $ isEmptyTChan cc) $ do
      schedule refreshList
      threadDelay 500

  fg `onKeyPressed` handleGlobal

  uiList st `onItemActivated` const shutdownUi

  uiInput st `onChange` updateList

  runUi (uiCollection st) defaultContext

  printSelection st
