 
module Graphics.SVGFonts.FontConverter.Types 
  ( LineWriter
  , runLW
  , writeToFile, writeToHandle
  , indent, line
  , dataPerFile
  ) where

import qualified Control.Monad.Writer as W

import System.IO ( withFile, hPutStrLn, hFlush, IOMode(..), Handle )

dataPerFile :: Int
dataPerFile = 30

-- -----------------------------------------------------------------------
-- Writer Monad
-- -----------------------------------------------------------------------

newtype LineWriter a = LW { unLW :: W.Writer [String] a }

instance Functor LineWriter where
  fmap f = LW . fmap f . unLW

instance Monad LineWriter where
  return = LW . return
  (>>=) m f = LW $ unLW m >>= unLW . f

instance Show (LineWriter a) where
  show w = unlines $ W.execWriter $ unLW w

line :: String -> LineWriter ()
line s = LW $ W.tell [s]

indent :: LineWriter a -> LineWriter a
indent w = LW $ W.censor (fmap ("  "++)) $ unLW w

runLW :: LineWriter a -> (a, [String])
runLW = W.runWriter . unLW

writeToFile :: FilePath -> LineWriter a -> IO a
writeToFile file w = withFile file WriteMode $ \h -> writeToHandle h w

writeToHandle :: Handle -> LineWriter a -> IO a
writeToHandle h w = do
  let (x, ls) = W.runWriter $ unLW w
  mapM_ (hPutStrLn h) ls
  hFlush h
  return x