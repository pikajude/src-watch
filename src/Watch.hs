{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}

module Watch where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List
import qualified Data.Map as M
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import qualified Filesystem.Path as FP
import System.Console.ANSI
import System.Directory
import System.FilePath
import System.FilePath.Glob
import System.FSNotify
import System.Posix.Files
import Watch.Investigate
import Watch.Logger
import Watch.Matches
import Watch.Options
import Watch.Spew

watch :: FilePath -> WatchOptions -> IO ()
watch root ops = withManager $ \ mgr -> do
    setCurrentDirectory root
    parent <- canonicalizePath root
    reset <- newEmptyMVar
    let l = runLogger (wLevel ops)
    forever $ do
        l $ quiet $ putStr "(Re)building dependency list..."
        concerns <- concernMap ops
        let ms = matches concerns
        l $ deafening $ putChar '\n' >> showConcerns concerns
        l $ quiet $ putStrLn "done."
        watchTree mgr (decodeString parent) (const True)
            (l . handleEv reset parent ms)
        l $ quiet $ putStrLn "Starting watch..."
        takeMVar reset

-- this is fine in theory. it's annoying because it doesn't support
-- ignoring events caused by the current process (like hfsevents does)
--
-- that means that we have to rebuild the dep tree for EVERY CHANGE
handleEv :: (MonadReader Verbosity m, MonadIO m)
         => MVar () -> FilePath -> M.Map FilePath [(Pattern, String)]
         -> Event -> m ()
handleEv reset root ms ev = do
    let rel = makeRelative root (encodeString $ eventPath ev)
        key = takeDirectory rel
        pat = takeFileName rel
    unless ("~" `isSuffixOf` pat) $ do
        if takeExtension pat == ".hs"
            then do
                loud $ showEvent False ev
                quiet $ putStrLn "Source file modified."
                liftIO $ putMVar reset ()
            else do
                let toTouch = map snd
                            . filter (\ (gl, _) -> gl `match` pat)
                            $ M.findWithDefault [] key ms
                loud $ showEvent (null toTouch) ev
                quiet $ case toTouch of
                    [] -> return ()
                    [x] -> putStrLn $ " forcing recompile for: " ++ x
                    xs -> do
                        putStrLn " forcing recompile for:"
                        mapM_ (putStrLn . ("  * " ++)) xs
                liftIO $ mapM_ touchFile toTouch

showEvent :: Bool -> Event -> IO ()
showEvent t (Removed fp _) = red "[removed] " >> dump t fp
showEvent t (Modified fp _) = yellow "[modified] " >> dump t fp
showEvent t (Added fp _) = green "[created] " >> dump t fp

dump :: Bool -> FP.FilePath -> IO ()
dump t (encodeString -> ev) = if t
    then putStrLn ev
    else color White (ev ++ "\n")

red, green, yellow :: String -> IO ()
red = color Red
green = color Green
yellow = color Yellow

color :: Color -> String -> IO ()
color r s = setSGR [SetColor Foreground Dull r] >> putStr s >> setSGR [Reset]
