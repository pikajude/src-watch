{-# LANGUAGE NoMonomorphismRestriction #-}

module Watch where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.List
import qualified Data.Map as M
import System.Console.ANSI
import System.Directory
import System.FilePath
import System.FilePath.Glob
import System.OSX.FSEvents
import System.Posix.Files
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
        l $ quiet $ putStr "(Re)building dependency list..."
        concerns <- concernMap opts
        let ms = matches concerns
        l $ deafening $ putChar '\n' >> showConcerns concerns
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

showEvent t ev
    | itemRemoved ev = pid ev >> red "[removed] " >> dump ev
    | itemModified ev = pid ev >> yellow "[modified] " >> dump ev
    | itemRenamed ev = pid ev >> yellow "[renamed] " >> dump ev
    | itemCreated ev = pid ev >> green "[created] " >> dump ev
    | mtime ev = pid ev >> yellow "[metadata] " >> dump ev
    | otherwise = pid ev >> putStrLn ("unknown event for " ++ eventPath ev)
    where dump ev = if t
                        then putStrLn (eventPath ev)
                        else color White (eventPath ev ++ "\n")
          pid ev = color Black (printf "%#0x " (eventId ev))

red = color Red
green = color Green
yellow = color Yellow

color r s = setSGR [SetColor Foreground Dull r] >> putStr s >> setSGR [Reset]

itemCreated ev = eventFlagItemCreated .&. eventFlags ev == eventFlagItemCreated
              && not (itemRemoved ev)
itemRemoved ev = eventFlagItemRemoved .&. eventFlags ev == eventFlagItemRemoved
itemRenamed ev = eventFlagItemRenamed .&. eventFlags ev == eventFlagItemRenamed
itemModified ev = eventFlagItemModified .&. eventFlags ev == eventFlagItemModified
isFile ev = eventFlagItemIsFile .&. eventFlags ev == eventFlagItemIsFile

mtime ev = eventFlagItemInodeMetaMod .&. eventFlags ev == eventFlagItemInodeMetaMod
