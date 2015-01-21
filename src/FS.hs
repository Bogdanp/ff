{-# LANGUAGE OverloadedStrings #-}

module FS (
    collect
  , fuzzySort
  , socketPath
) where

import           Control.Applicative          ((<$>), (<*>))
import           Control.Concurrent.STM       (STM, TMVar, atomically, putTMVar,
                                               takeTMVar)
import           Control.Monad                (liftM, (>=>))
import           Data.List                    (isPrefixOf, isSuffixOf)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Vector                  (Vector, filterM, mapM, mapM_)
import qualified Data.Vector                  as V
import           Data.Vector.Algorithms.Intro (sortBy)
import           Prelude                      hiding (mapM, mapM_)
import           System.Directory             (doesDirectoryExist,
                                               getDirectoryContents,
                                               getUserDocumentsDirectory)
import           System.FilePath              ((</>))


modifyTMVar :: TMVar a -> (a -> a) -> STM ()
modifyTMVar v f = takeTMVar v >>= putTMVar v . f


-- | Recursively collects every file under `base` into the TMVar.
collect :: TMVar (Vector Text) -> Text -> IO ()
collect cs base = do
  fs <- qualify =<< contentsOf base
  atomically $ modifyTMVar cs $ flip (<>) fs
  directories fs
 where qualify     = mapM $ return . ((base <> "/") <>)
       directories = filterM (doesDirectoryExist . T.unpack) >=> mapM_ (collect cs)


contentsOf :: Text -> IO (Vector Text)
contentsOf = contentsOf' >=> validate >=> pack
 where contentsOf' = liftM V.fromList . getDirectoryContents . T.unpack
       validFile   = (&&) <$> (not . isPrefixOf ".") <*> (not . isSuffixOf ".pyc")
       validate    = filterM $ return . validFile
       pack        = mapM $ return . T.pack


fuzzyMatch :: String -> Text -> (Bool, Double, Text)
fuzzyMatch s p = fuzzyMatch' s p 0


fuzzyMatch' :: String -> Text -> Double -> (Bool, Double, Text)
fuzzyMatch'    []  p n = (True, n, p)
fuzzyMatch' (x:xs) p n = maybe (False, n, p) match $ x `elemIndex` p
 where match i   = let (b', n', p') = fuzzyMatch' xs (T.drop i p) (n + fromIntegral i)
                    in (b', n', p)
       elemIndex = T.findIndex . (==)


fuzzySort :: String -> Vector Text -> Vector Text
fuzzySort s xs = V.map ext $ V.modify (sortBy cmp) $ V.filter flt $ V.map (fuzzyMatch s) xs
 where cmp (_, n, _) (_, n', _) = n `compare` n'
       flt (b, _, _) = b
       ext (_, _, t) = t


socketPath :: IO FilePath
socketPath = liftM (</> ".ff.socket") getUserDocumentsDirectory
