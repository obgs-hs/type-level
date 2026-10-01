module Main where

import System.Environment (getArgs)
import System.IO (hSetEncoding, stderr, stdout, utf8)
import System.Info (os)
import Test.DocTest (mainFromCabal)

-- | GHC quotes names in its diagnostics with typographic quotation marks, and
-- doctest reports those diagnostics verbatim. The default Windows console
-- encoding cannot represent them, so without this the run throws partway with
-- no summary and looks like a shorter, greener one.
main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  mainFromCabal "obgs-type-level" . (windowsArgs ++) =<< getArgs

-- | doctest-parallel 0.4.1 sets up the GHC sessions of all its workers at once,
-- which on Windows makes them collide on the package.cache.lock files and die.
-- Running a single worker avoids it. Drop this once a release serializes the
-- setup.
windowsArgs :: [String]
windowsArgs = ["-j1" | os == "mingw32"]
