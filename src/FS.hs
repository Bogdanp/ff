{-# LANGUAGE OverloadedStrings #-}

module FS (
    collect
  , fuzzyMatch
  , socketPath
) where

import           Control.Applicative    ((<$>), (<*>))
import           Control.Concurrent.STM (TMVar, atomically, putTMVar, takeTMVar)
import           Control.Monad          (liftM, (>=>))
import           Data.List              (isPrefixOf, isSuffixOf)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Vector            (Vector, filterM, mapM, mapM_)
import qualified Data.Vector            as V
import           Prelude                hiding (mapM, mapM_)
import           System.Directory       (doesDirectoryExist,
                                         getDirectoryContents,
                                         getUserDocumentsDirectory)
import           System.FilePath        ((</>))


--------------------------------------------------------------------------------
-- | Recursively collects every path under the given path into the TMVar.
collect :: TMVar (Vector Text) -> Text -> IO ()
collect cs base = do
  fs <- qualify =<< contentsOf base
  atomically $ takeTMVar cs >>= putTMVar cs . flip (<>) fs
  directories fs
 where qualify     = mapM $ return . ((base <> "/") <>)
       directories = filterM (doesDirectoryExist . T.unpack)  >=> mapM_ (collect cs)

--------------------------------------------------------------------------------
-- |
contentsOf :: Text -> IO (Vector Text)
contentsOf = contentsOf' >=> validate >=> pack
 where contentsOf' = liftM V.fromList . getDirectoryContents . T.unpack
       validFile   = (&&) <$> (not . isPrefixOf ".") <*> (not . isSuffixOf ".pyc")
       validate    = filterM $ return . validFile
       pack        = mapM $ return . T.pack


--------------------------------------------------------------------------------
-- |
fuzzyMatch :: String -> Text -> Bool
fuzzyMatch    []  _ = True
fuzzyMatch (x:xs) p = maybe False match $ x `elemIndex` p
 where match i   = fuzzyMatch xs $ T.drop i p
       elemIndex = T.findIndex . (==)

--------------------------------------------------------------------------------
-- |
socketPath :: IO FilePath
socketPath = liftM (</> ".ff.socket") getUserDocumentsDirectory
