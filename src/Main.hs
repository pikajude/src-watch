import Data.Foldable
import Data.List.NonEmpty
import qualified Data.Map as M
import Options
import System.Console.ANSI
import System.Directory
import System.Exit
import System.FilePath
import System.FilePath.Glob
import System.IO
import Watch.Investigate
import Watch.Matches

main :: IO ()
main = runCommand $ \ opts args ->
    case args of
        [abs'] -> do
            setCurrentDirectory abs'
            let comp = uncurry (\ a b -> a </> decompile b)
            cMap <- concernMap opts
            forM_ (M.toList cMap) $ \ (file, (d :| ds)) -> do
                setSGR [SetColor Foreground Dull Yellow]
                putStrLn file
                setSGR [SetColor Foreground Dull Black]
                putStr " -> "
                setSGR [Reset]
                putStrLn (comp d)
                forM_ ds $ \ t -> do
                    putStrLn $ "    " ++ comp t
            print $ matches cMap
        [] -> panic "Usage: src-watch [-i ignores] path"
        _ -> panic "Extra rubbish on end of arguments, expected exactly one path"

panic :: String -> IO a
panic s = do
    hPutStrLn stderr s
    exitFailure
