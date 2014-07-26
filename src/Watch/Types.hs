module Watch.Types where

import Data.List.NonEmpty as N
import Data.Map as M
import System.FilePath.Glob

type Refs = M.Map FilePath Matches

type Match = (FilePath, Pattern)
type Matches = N.NonEmpty Match
