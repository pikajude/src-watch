module Watch.Matches where

import qualified Data.List.NonEmpty as N
import qualified Data.Map as M
import System.FilePath.Glob
import Watch.Types

-- we use trie search to find the parent dir then match on pattern
-- left is pattern, right is file that worries about it
type Filter = [(Pattern, FilePath)]

matches :: Refs -> M.Map FilePath Filter
matches = M.fromListWith (++) . concatMap fromPair . M.toList

fromPair :: (FilePath, Matches) -> [(FilePath, Filter)]
fromPair (fp, ms) = map (\ (target, pat) -> (target, [(pat, fp)]))
                  $ N.toList ms
