{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Watch.Investigate where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import qualified Data.List.NonEmpty as N
import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup
import qualified Data.Set as S
import Language.Haskell.Exts hiding (Match)
import Language.Preprocessor.Cpphs
import System.Directory
import System.FilePath
import System.FilePath.Glob
import Watch.Options hiding ((<>))
import Watch.References
import Watch.Types

known :: Field1 s s (S.Set FilePath) (S.Set FilePath) => Lens' s (S.Set FilePath)
known = _1

refs :: Field2 s s Refs Refs => Lens' s Refs
refs = _2

concernMap :: MonadIO m => WatchOptions -> m Refs
concernMap wopts = do
    ignores <- mapM (liftIO . canonicalizePath) (wIgnores wopts)
    (_, tree) <- (`runReaderT` (wopts { wIgnores = ignores }, ()))
        . (`execStateT` (S.fromList [], M.fromList []))
        $ investigateDir "."
    return $ fmap N.nub tree

investigateDir :: (MonadState s m, MonadReader r m, MonadIO m,
                   Field1 r r WatchOptions WatchOptions,
                   Field1 s s (S.Set FilePath) (S.Set FilePath),
                   Field2 s s Refs Refs)
               => FilePath -> m ()
investigateDir dest = do
    ks <- use known
    if dest `S.member` ks
        then error "Symlink cycle detected!"
        else known %= S.insert dest
    contents' <- liftIO $ liftM (\\ [".", ".."]) (getDirectoryContents dest)
    let contents = map (normalise . (dest </>)) contents'
    forM_ contents $ \ item -> do
        isDirectory <- liftIO $ doesDirectoryExist item
        isFile <- liftIO $ doesFileExist item
        case () of
            _ | isDirectory -> investigateDir item
              | isFile -> investigateFile item
              | otherwise -> error $ "Found item " ++ item
                                  ++ ", which is neither file nor directory."

investigateFile :: (MonadState s m, MonadReader r m, MonadIO m,
                    Field1 r r WatchOptions WatchOptions,
                    Field2 s s Refs Refs)
                => FilePath -> m ()
investigateFile f
    | takeExtension f == ".hs" = investigateHaskell f
    | otherwise = return ()

investigateHaskell :: (MonadState s m, MonadReader r m, MonadIO m,
                       Field1 r r WatchOptions WatchOptions,
                       Field2 s s Refs Refs)
                   => FilePath -> m ()
investigateHaskell file = do
    defs <- asks (wDefines . view _1)
    pp <- liftIO (readFile file >>= cpp defs file)
    let result = parseModuleWithMode (lenientParseMode file) pp
    case result of
        ParseFailed lc str -> liftIO $ print (lc, str)
        ParseOk m -> forM_ (splices m) $ \ splice -> do
            let ps = findPrefixes
            paths <- liftIO $ mapM (spliceSearch splice) ps
            forM_ (concat paths) (addRef file)

spliceSearch :: String -> FilePath -> IO [(FilePath, Pattern)]
spliceSearch s prefix = do
    let pats = map compile [s, s ++ ".*"]
    (matches, _) <- globDir pats prefix
    return . catMaybes $ zipWith (\ p m -> if null m
        then Nothing
        else Just (prefixize prefix p))
            pats matches

-- strips anything that looks like a directory off the front of the pattern
-- and puts it on the end of the path
prefixize :: FilePath -> Pattern -> (FilePath, Pattern)
prefixize parent pat = first normalise $
        commonDirectory (compile (escape parent </> decompile pat)) where
    escape = concatMap escapeChar
    escapeChar c | c `elem` "[]*<>^!?" = ['[', c, ']']
                 | otherwise = [c]

findPrefixes :: [FilePath]
findPrefixes = [".", "templates"]

addRef :: (MonadState s m, Field2 s s Refs Refs)
       => FilePath -> (FilePath, Pattern) -> m ()
addRef file expr = refs %= M.insertWith (<>) file (expr N.:| [])

lenientParseMode :: String -> ParseMode
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

cpp :: [(String, String)] -> FilePath -> String -> IO String
cpp defs = runCpphs defaultCpphsOptions
    { boolopts = defaultBoolOptions { hashline = False }
    , defines = defs
    }
