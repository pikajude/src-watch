{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}

module Watch where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.List
import qualified Data.Map as M
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import System.Console.ANSI
import System.Directory
import System.FilePath
import System.FilePath.Glob
import System.FSNotify
import System.Posix.Files
import Text.Printf
import Watch.Investigate
import Watch.Logger
import Watch.Matches
import Watch.Options
import Watch.Spew

watch root opts = withManager $ \ mgr -> do
    setCurrentDirectory root
    parent <- canonicalizePath root
    reset <- newEmptyMVar
    let l = runLogger (wLevel opts)
    forever $ do
        l $ quiet $ putStr "(Re)building dependency list..."
        concerns <- concernMap opts
        let ms = matches concerns
        l $ deafening $ putChar '\n' >> showConcerns concerns
        l $ quiet $ putStrLn "done."
        watchTree mgr (decodeString parent) (const True)
            (l . handleEv reset parent ms)
        l $ quiet $ putStrLn "Starting watch..."
        takeMVar reset

-- this is fine in theory. it's annoying because it doesn't support
-- ignoring events caused by the current process (like hfsevents does)
handleEv reset root matches ev = do
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
                            $ M.findWithDefault [] key matches
                loud $ showEvent (null toTouch) ev
                quiet $ case toTouch of
                    [] -> return ()
                    [x] -> putStrLn $ " forcing recompile for: " ++ x
                    xs -> do
                        putStrLn " forcing recompile for:"
                        mapM_ (putStrLn . ("  * " ++)) xs
                liftIO $ mapM_ touchFile toTouch

showEvent t (Removed fp _) = red "[removed] " >> dump t fp
showEvent t (Modified fp _) = yellow "[modified] " >> dump t fp
showEvent t (Added fp _) = green "[created] " >> dump t fp

dump t (encodeString -> ev) = if t
    then putStrLn ev
    else color White (ev ++ "\n")

red = color Red
green = color Green
yellow = color Yellow

color r s = setSGR [SetColor Foreground Dull r] >> putStr s >> setSGR [Reset]
