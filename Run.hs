-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Run
-- Copyright   :  (C) 2007 Spencer Janssen, Andrea Rossato, glasser@mit.edu
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Christian Thiemann <mail@christian-thiemann.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This modules provides several commands to run an external process.
-- It is composed of functions formerly defined in XMonadContrib.Dmenu (by
-- Spenver Jannsen), XMonadContrib.Dzen (by glasser@mit.edu) and
-- XMonadContrib.RunInXTerm (by Andrea Rossato).
--
-----------------------------------------------------------------------------

module XMonadContrib.Run (
                          -- * Usage
                          -- $usage
                          runProcessWithInput,
                          runProcessWithInputAndWait,
                          seconds
                         ) where

import Control.Monad.State (Monad((>>), return), when)
import System.Posix.Process (createSession, forkProcess, executeFile,
			    getProcessStatus)
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.IO (IO, FilePath, hPutStr, hGetContents, hFlush, hClose)
import System.Process (runInteractiveProcess, waitForProcess)
import XMonad (X, io, spawn)

-- $usage
-- For an example usage of runInXTerm see XMonadContrib.SshPrompt
--
-- For an example usage of runProcessWithInput see
-- XMonadContrib.{DirectoryPrompt,Dmenu,ShellPrompt,WmiiActions,WorkspaceDir}
--
-- For an example usage of runProcessWithInputAndWait see XMonadContrib.Dzen

-- | Returns Just output if the command succeeded, and Nothing if it didn't.
-- This corresponds to dmenu's notion of exit code 1 for a cancelled invocation.
runProcessWithInput :: FilePath -> [String] -> String -> IO String
runProcessWithInput cmd args input = do
    (pin, pout, perr, ph) <- runInteractiveProcess cmd args Nothing Nothing
    hPutStr pin input
    hClose pin
    output <- hGetContents pout
    when (output==output) $ return ()
    hClose pout
    hClose perr
    waitForProcess ph
    return output

-- wait is in us
runProcessWithInputAndWait :: FilePath -> [String] -> String -> Int -> IO ()
runProcessWithInputAndWait cmd args input timeout = do
    pid <- forkProcess $ do
       forkProcess $ do -- double fork it over to init
         createSession
         (pin, pout, perr, ph) <- runInteractiveProcess cmd args Nothing Nothing
         hPutStr pin input
         hFlush pin
         threadDelay timeout
         hClose pin
         hClose pout
         hClose perr
         waitForProcess ph
         return ()
       exitWith ExitSuccess
       return ()
    getProcessStatus True False pid
    return ()

{- | Multiplies by ONE MILLION, for use with runProcessWithInputAndWait.
     Use like:
     > (5.5 `seconds`)
-}
seconds :: Rational -> Int
seconds = fromEnum . (* 1000000)
