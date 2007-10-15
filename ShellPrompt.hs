-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.ShellPrompt
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A shell prompt for XMonad
--
-----------------------------------------------------------------------------

module XMonadContrib.ShellPrompt (
                             -- * Usage
                             -- $usage
                             shellPrompt
                             , getShellCompl
                             , split
                              ) where

import XMonad
import XMonadContrib.XPrompt
import XMonadContrib.Dmenu

import Control.Monad
import Data.List
import Data.Set (toList, fromList)
import System.Directory
import System.IO
import System.Environment

-- $usage
--
-- 1. In Config.hs add:
--
-- > import XMonadContrib.XPrompt
-- > import XMonadContrib.ShellPrompt
--
-- 2. In your keybindings add something like:
--
-- >   , ((modMask .|. controlMask, xK_x), shellPrompt defaultXPConfig)
--

-- %import XMonadContrib.XPrompt
-- %import XMonadContrib.ShellPrompt
-- %keybind , ((modMask .|. controlMask, xK_x), shellPrompt defaultXPConfig)

data Shell = Shell

instance XPrompt Shell where
    showXPrompt Shell = "Run:   "


shellPrompt :: XPConfig -> X ()
shellPrompt c = mkXPrompt Shell c getShellCompl spawn

getShellCompl :: String -> IO [String]
getShellCompl s
    | s /= "" && last s /= ' ' = do
  f <- fmap lines $ runProcessWithInput "/bin/bash" [] ("compgen -A file " ++ s ++ "\n")
  c <- commandCompletionFunction s
  return . map escape . sort . (toList . fromList) $ f ++ c
    | otherwise = return []

commandCompletionFunction :: String -> IO [String]
commandCompletionFunction str
    | '/' `elem` str = return []
    | otherwise      = do
  p  <- getEnv "PATH" `catch` const (return [])
  let ds = split ':' p
      fp d f = d ++ "/" ++ f
  es <- forM ds $ \d -> do
          exists <- doesDirectoryExist d
          if exists
             then getDirectoryContents d >>= filterM (isExecutable . fp d)
             else return []
  return . filter (isPrefixOf str) . concat $ es

isExecutable :: FilePath ->IO Bool
isExecutable f = do
    fe <- doesFileExist f
    if fe
        then fmap executable $ getPermissions f
        else return False

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l =
    f : split e (rest ls)
        where
          (f,ls) = span (/=e) l
          rest s | s == []   = []
                 | otherwise = tail s

escape :: String -> String
escape []       = ""
escape (' ':xs) = "\\ " ++ escape xs
escape (x:xs)
    | isSpecialChar x = '\\' : x : escape xs
    | otherwise       = x : escape xs

isSpecialChar :: Char -> Bool
isSpecialChar =  flip elem "\\@\"'#?$*()[]{};"
