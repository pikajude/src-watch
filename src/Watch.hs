{-# LANGUAGE NoMonomorphismRestriction #-}

module Watch where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import qualified Data.Map as M
import System.Console.ANSI
import System.Directory
import System.FilePath
import System.FilePath.Glob
import System.OSX.FSEvents
import Text.Printf
import Watch.Investigate
import Watch.Logger
import Watch.Matches
import Watch.Options
import Watch.Spew

watch root opts = do
    setCurrentDirectory root
    parent <- canonicalizePath root
    reset <- newEmptyMVar
    let l = runLogger (wLevel opts)
    forever $ do
        l $ quiet $ putStr "Building dependency list..."
        concerns <- concernMap opts
        let ms = matches concerns
        l $ loud $ showConcerns concerns
        l $ quiet $ putStrLn "done."
        stream <- eventStreamCreate [parent] 1.0 True True True
            (l . handleEv reset parent ms)
        l $ quiet $ putStrLn "Starting watch..."
        () <- takeMVar reset
        eventStreamDestroy stream

handleEv reset root matches ev = do
    let rel = makeRelative root (eventPath ev)
        key = takeDirectory rel
        pat = takeFileName rel
    loud $ showEvent ev
    let toTouch = map snd
                . filter (\ (gl, _) -> gl `match` pat)
                $ M.findWithDefault [] key matches
    unless (null toTouch) $ quiet $ do
        putStrLn " forcing recompile for:"
        mapM_ (putStrLn . ("  * " ++)) toTouch

showEvent ev
    | itemRemoved ev = pid ev >> red "[removed] " >> dump ev
    | itemModified ev = pid ev >> yellow "[modified] " >> dump ev
    | itemRenamed ev = pid ev >> yellow "[renamed] " >> dump ev
    | itemCreated ev = pid ev >> green "[created] " >> dump ev
    | mtime ev = pid ev >> yellow "[metadata] " >> dump ev
    | otherwise = pid ev >> putStrLn ("unknown event for " ++ eventPath ev)
    where dump ev = putStrLn (ftype ev ++ " " ++ eventPath ev)
          pid ev = color Black (printf "%#0x " (eventId ev))

red = color Red
green = color Green
yellow = color Yellow

color r s = setSGR [SetColor Foreground Dull r] >> putStr s >> setSGR [Reset]

ftype e = if isFile e then "file" else "directory"

itemCreated ev = eventFlagItemCreated .&. eventFlags ev == eventFlagItemCreated
              && not (itemRemoved ev)
itemRemoved ev = eventFlagItemRemoved .&. eventFlags ev == eventFlagItemRemoved
itemRenamed ev = eventFlagItemRenamed .&. eventFlags ev == eventFlagItemRenamed
itemModified ev = eventFlagItemModified .&. eventFlags ev == eventFlagItemModified
isFile ev = eventFlagItemIsFile .&. eventFlags ev == eventFlagItemIsFile

mtime ev = eventFlagItemInodeMetaMod .&. eventFlags ev == eventFlagItemInodeMetaMod
