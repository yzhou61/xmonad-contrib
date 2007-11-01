-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Commands
-- Copyright   :  (c) David Glasser 2007
-- License     :  BSD3
-- 
-- Maintainer  :  glasser@mit.edu
-- Stability   :  stable
-- Portability :  portable
--
-- Allows you to run internal xmonad commands (X () actions) using
-- a dmenu menu in addition to key bindings.  Requires dmenu and
-- the Dmenu XMonadContrib module.
--
-----------------------------------------------------------------------------

module XMonadContrib.Commands (
                             -- * Usage
                             -- $usage
                             commandMap,
                             runCommand,
                             runCommand',
                             workspaceCommands,
                             screenCommands,
                             defaultCommands
                              ) where

import XMonad
import XMonad.Operations
import XMonad.StackSet hiding (workspaces)
import XMonadContrib.Dmenu (dmenu)
import XMonad.Layouts

import Control.Monad.Reader
import qualified Data.Map as M
import System.Exit
import Data.Maybe

-- $usage
--
-- To use, modify your Config.hs to:
--
-- >    import XMonadContrib.Commands
--
-- and add a keybinding to the runCommand action:
--
-- >    , ((modMask .|. controlMask, xK_y), runCommand commands)
--
-- and define the list commands:
--
-- >    commands :: [(String, X ())]
-- >    commands = defaultCommands
--
-- A popup menu of internal xmonad commands will appear.  You can
-- change the commands by changing the contents of the list
-- 'commands'.  (If you like it enough, you may even want to get rid
-- of many of your other key bindings!)

-- %def commands :: [(String, X ())]
-- %def commands = defaultCommands
-- %import XMonadContrib.Commands
-- %keybind , ((modMask .|. controlMask, xK_y), runCommand commands)

commandMap :: [(String, X ())] -> M.Map String (X ())
commandMap c = M.fromList c

workspaceCommands :: X [(String, X ())]
workspaceCommands = asks (workspaces . config) >>= \spaces -> return
                            [((m ++ show i), windows $ f i)
                                | i <- spaces
                                , (f, m) <- [(view, "view"), (shift, "shift")] ]

screenCommands :: [(String, X ())]
screenCommands = [((m ++ show sc), screenWorkspace (fromIntegral sc) >>= flip whenJust (windows . f))
                      | sc <- [0, 1]::[Int] -- TODO: adapt to screen changes
                      , (f, m) <- [(view, "screen"), (shift, "screen-to-")]
                 ]

defaultCommands :: X [(String, X ())]
defaultCommands = do
    wscmds <- workspaceCommands
    return $ wscmds ++ screenCommands ++ otherCommands
 where
    sr = broadcastMessage ReleaseResources
    otherCommands =
        [ ("shrink"              , sendMessage Shrink                               )
        , ("expand"              , sendMessage Expand                               )
        , ("next-layout"         , sendMessage NextLayout                           )
        , ("default-layout"      , asks (layoutHook . config) >>= setLayout         )
        , ("restart-wm"          , sr >> restart Nothing True                       )
        , ("restart-wm-no-resume", sr >> restart Nothing False                      )
        , ("xterm"               , spawn =<< asks (terminal .  config)              )
        , ("run"                 , spawn "exe=`dmenu_path | dmenu -b` && exec $exe" )
        , ("kill"                , kill                                             )
        , ("refresh"             , refresh                                          )
        , ("focus-up"            , windows $ focusUp                                )
        , ("focus-down"          , windows $ focusDown                              )
        , ("swap-up"             , windows $ swapUp                                 )
        , ("swap-down"           , windows $ swapDown                               )
        , ("swap-master"         , windows $ swapMaster                             )
        , ("sink"                , withFocused $ windows . sink                     )
        , ("quit-wm"             , io $ exitWith ExitSuccess                        )
        ]

runCommand :: [(String, X ())] -> X ()
runCommand cl = do
  let m = commandMap cl
  choice <- dmenu (M.keys m)
  fromMaybe (return ()) (M.lookup choice m)

runCommand' :: String -> X ()
runCommand' c = do
  m <- fmap commandMap defaultCommands
  fromMaybe (return ()) (M.lookup c m)
