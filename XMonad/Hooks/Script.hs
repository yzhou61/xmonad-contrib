-----------------------------------------------------------------------------
-- |
-- Module      : XMonad.Hooks.Script
-- Copyright   : (c) Trevor Elliott <trevor@galois.com>
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Trevor Elliott <trevor@galois.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Provides a simple interface for running a ~\/.xmonad\/hooks script with the
-- name of a hook.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.Script (
    -- * Usage
    -- $usage

    -- * Script Hook Interface
    execScriptHook
  ) where

--
-- Useful Imports
--
import XMonad

import Control.Monad.Trans
import System.Directory

-- $usage
--
-- This module allows you to run a centrally located script with the text
-- name of a hook.  The script is assumed to be located at @~\/.xmonad\/hooks@.
--
-- For example, if you wanted to run the hook "startup" in your script every
-- time your startup hook ran, you could modify your xmonad config as such:
--
-- > main = xmonad $ defaultConfig {
-- >   ...
-- >   startupHook = execScriptHook "startup"
-- >   ...
-- >   }
--
-- Now, every time the startup hook runs, the command
-- @~\/.xmonad\/hooks startup@ will also.

-- | Execute a named script hook
execScriptHook :: MonadIO m => String -> m ()
execScriptHook hook = io $ do
  home <- getHomeDirectory
  let script = home ++ "/.xmonad/hooks "
  spawn (script ++ hook)
