module FS (
    collect
  , fuzzyMatch
  , socketPath
) where

import           Control.Applicative    ((<$>), (<*>))
import           Control.Concurrent.STM (TChan, atomically, writeTChan)
import           Control.Monad          (filterM, liftM, (>=>))
import           Data.List              (elemIndex, isPrefixOf, isSuffixOf)
import           System.Directory       (doesDirectoryExist,
                                         getDirectoryContents,
                                         getUserDocumentsDirectory)
import           System.FilePath        ((</>))

collect :: TChan [FilePath] -> FilePath -> IO ()
collect cc base = do
  fs <- qualify =<< contentsOf
  atomically $ writeTChan cc fs
  directories fs
 where qualify     = mapM $ return . (base </>)
       validFile   = (&&) <$> (not . isPrefixOf ".") <*> (not . isSuffixOf ".pyc")
       contentsOf  = filterM (return . validFile) =<< getDirectoryContents base
       directories = filterM doesDirectoryExist >=> mapM_ (collect cc)

fuzzyMatch :: String -> FilePath -> Bool
fuzzyMatch    []  _ = True
fuzzyMatch (x:xs) p = maybe False match $ x `elemIndex` p
  where match i = fuzzyMatch xs $ drop i p

socketPath :: IO FilePath
socketPath = liftM (</> ".ff.socket") getUserDocumentsDirectory
