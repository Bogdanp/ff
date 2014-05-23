module FS (
    collect
  , fuzzyMatch
  , socketPath
) where

import           Control.Applicative    ((<$>), (<*>))
import           Control.Concurrent.STM (TMVar, atomically, putTMVar, takeTMVar)
import           Control.Monad          (filterM, liftM, (>=>))
import           Data.List              (elemIndex, isPrefixOf, isSuffixOf)
import           System.Directory       (doesDirectoryExist,
                                         getDirectoryContents,
                                         getUserDocumentsDirectory)
import           System.FilePath        ((</>))


--------------------------------------------------------------------------------
-- | Recursively collects every path under the given path into the TMVar.
collect :: TMVar [FilePath] -> FilePath -> IO ()
collect cs base = do
  fs <- qualify =<< contentsOf
  atomically $ takeTMVar cs >>= putTMVar cs . flip (++) fs
  directories fs
 where qualify     = mapM $ return . (base </>)
       validFile   = (&&) <$> (not . isPrefixOf ".") <*> (not . isSuffixOf ".pyc")
       contentsOf  = filterM (return . validFile) =<< getDirectoryContents base
       directories = filterM doesDirectoryExist >=> mapM_ (collect cs)

fuzzyMatch :: String -> FilePath -> Bool
fuzzyMatch    []  _ = True
fuzzyMatch (x:xs) p = maybe False match $ x `elemIndex` p
  where match i = fuzzyMatch xs $ drop i p

socketPath :: IO FilePath
socketPath = liftM (</> ".ff.socket") getUserDocumentsDirectory
