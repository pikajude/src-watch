module Watch.Spew where

import Data.Foldable
import Data.List.NonEmpty
import qualified Data.Map as M
import System.Console.ANSI
import System.FilePath
import System.FilePath.Glob
import Watch.Types

showConcerns :: Refs -> IO ()
showConcerns cMap =
    forM_ (M.toList cMap) $ \ (file, d :| ds) -> do
        setSGR [SetColor Foreground Dull Yellow]
        putStrLn file
        setSGR [SetColor Foreground Dull Black]
        putStr " -> "
        setSGR [Reset]
        putStrLn (comp d)
        forM_ ds $ \ t -> putStrLn $ "    " ++ comp t
    where comp = uncurry (\ a b -> a </> decompile b)
