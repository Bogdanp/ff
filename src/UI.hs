{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module UI ( uiMain ) where

import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Concurrent.STM   (atomically, newTMVar, readTMVar,
                                           swapTMVar)
import           Control.Monad            (liftM, when)
import           Control.Monad.Loops      (whileM_)
import qualified Data.Text                as T
import           Graphics.Vty
import           Graphics.Vty.Widgets.All
import           System.Directory         (doesDirectoryExist)
import           System.Exit              (exitFailure, exitSuccess)

import           FS                       (collect, fuzzyMatch, socketPath)


--------------------------------------------------------------------------------
-- ATTRIBUTES
--------------------------------------------------------------------------------
-- | How list items look.
liAttr :: Attr
liAttr = black `on` white

--------------------------------------------------------------------------------
-- | How the text input looks.
inputAttr :: Attr
inputAttr = white `on` black


--------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------
-- | Represents the term UI.
data UI = UI { uiList       :: Widget (List T.Text FormattedText)
             , uiProgress   :: Widget FormattedText
             , uiInput      :: Widget Edit
             , uiCollection :: Collection }

--------------------------------------------------------------------------------
-- | Returns the initial state of the UI.
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

--------------------------------------------------------------------------------
-- | Writes the current selection to the socket.
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

--------------------------------------------------------------------------------
-- | Returns the height of the current terminal.
terminalHeight :: IO Int
terminalHeight = liftM (fromIntegral . region_height) $ terminal_handle >>= display_bounds

uiMain :: IO ()
uiMain = do
  cs         <- atomically $ newTMVar []
  collecting <- atomically $ newTMVar True

  th <- terminalHeight
  st <- initialUI
  ui <- centered =<<      return (uiList st)
                     <--> return (uiProgress st)
                     <--> return (uiInput st)

  fg <- newFocusGroup
  _  <- addToCollection (uiCollection st) ui fg
  _  <- addToFocusGroup fg $ uiInput st
  _  <- addToFocusGroup fg $ uiList st

  let query :: String -> [FilePath] -> [FilePath]
      query = filter . fuzzyMatch

      updateList :: T.Text -> IO ()
      updateList t = do
        mli <- getSelected $ uiList st
        xs  <- atomically $ readTMVar cs
        let fs = query (T.unpack t) xs
            pg = show (length fs) ++ "/" ++ show (length xs)
        setText (uiProgress st) $ T.pack pg
        clearList $ uiList st
        mapM_ appendItem $ take (th - 2) fs
        case mli of
          Nothing     -> return ()
          Just (i, _) -> do
            size <- getListSize $ uiList st
            when (size > i) $ setSelected (uiList st) i

      refreshList :: IO ()
      refreshList = do
        queryString <- getEditText $ uiInput st
        schedule $ updateList queryString
        return ()

      appendItem :: String -> IO ()
      appendItem s = let text = T.pack s
                      in plainText text >>= addToList (uiList st) text

      handleGlobal :: a -> Key -> [Modifier] -> IO Bool
      handleGlobal _ k ms | k == KEnter     = activateCurrentItem (uiList st) >> return False
                          | MCtrl `elem` ms = handleCtrl k >> return False
                          | otherwise       = return False

      handleCtrl :: Key -> IO ()
      handleCtrl k | k == KASCII 'c' = exitFailure
                   | k == KASCII 'p' = scrollUp $ uiList st
                   | k == KASCII 'n' = scrollDown $ uiList st
                   | k == KASCII 'w' = setEditText (uiInput st) T.empty
                   | otherwise       = return ()

  forkIO $ do
    collect cs "."
    atomically $ swapTMVar collecting False
    refreshList
    return ()

  forkIO $
    whileM_ (atomically $ readTMVar collecting) $ do
      refreshList
      threadDelay 250000

  fg `onKeyPressed` handleGlobal

  uiList st `onItemActivated` const shutdownUi

  uiInput st `onChange` updateList

  runUi (uiCollection st) defaultContext

  printSelection st
