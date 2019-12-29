{-# LANGUAGE TemplateHaskell #-}

module Smos.Version where

import Data.List

import GitHash

import Language.Haskell.TH

versionMessage :: String
versionMessage = intercalate "\n" $ concat [[giHash gi, giCommitDate gi], ["(dirty)" | giDirty gi]]
  where
    gi = $(unType <$> tGitInfoCwd)
