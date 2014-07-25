{-# LANGUAGE NoMonomorphismRestriction #-}

module Watch.Investigate where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import Language.Haskell.Exts
import Language.Preprocessor.Cpphs
import System.Directory
import System.FilePath
import Watch.Options
import Watch.References

known = _1
refs = _2

-- findConcernedFiles :: FilePath -> WatchOptions -> IO ()
findConcernedFiles source wopts = do
    ignores <- mapM canonicalizePath (wIgnores wopts)
    (_, tree) <- (`runReaderT` wopts { wIgnores = ignores })
        . (`execStateT` (S.fromList [], M.fromList []))
        $ investigateDir source
    return tree

investigateDir d = do
    dest <- liftIO (canonicalizePath d)
    ks <- use known
    if dest `S.member` ks
        then error "Symlink cycle detected!"
        else known %= (S.insert dest)
    contents <- liftIO $ liftM (\\ [".", ".."]) (getDirectoryContents dest)
    forM_ contents $ \ item' -> do
        item <- liftIO (canonicalizePath (dest </> item'))
        isDirectory <- liftIO $ doesDirectoryExist item
        isFile <- liftIO $ doesFileExist item
        case () of
            _ | isDirectory -> investigateDir item
              | isFile -> investigateFile item
              | otherwise -> error $ "Found item " ++ item
                                  ++ ", which is neither file nor directory."

investigateFile f
    | takeExtension f == ".hs" = investigateHaskell f
    | otherwise = return ()

investigateHaskell file = do
    defs <- asks wDefines
    pp <- liftIO (readFile file >>= cpp defs file)
    let result = parseModuleWithMode (lenientParseMode file) pp
    case result of
        ParseFailed lc str -> error (show (lc, str))
        ParseOk m -> forM_ (splices m) (addRef file)

addRef file exp = refs %= M.insertWith (<>) file (S.singleton exp)

lenientParseMode f = defaultParseMode
    { extensions = [ EnableExtension FlexibleContexts
                   , EnableExtension MultiParamTypeClasses
                   , EnableExtension PackageImports
                   , EnableExtension QuasiQuotes
                   , EnableExtension ScopedTypeVariables
                   , EnableExtension StandaloneDeriving
                   , EnableExtension TemplateHaskell
                   , EnableExtension TypeFamilies
                   , EnableExtension ViewPatterns
                   ]
    , parseFilename = f
    , fixities = Just (baseFixities ++ lensFixities)
    }
    where
        lensFixities = infixl_ 1 ["&"]
                    ++ infixr_ 4 [".~", "+~", "%~"]

cpp defs = runCpphs defaultCpphsOptions
    { boolopts = defaultBoolOptions { hashline = False }
    , defines = defs
    }
