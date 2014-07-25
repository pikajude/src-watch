import Options
import System.Directory
import System.Exit
import System.IO
import Watch.Investigate
import Watch.Options

main :: IO ()
main = runCommand $ \ opts args ->
    case args of
        [path] -> do
            setCurrentDirectory (wBasedir opts)
            concerns <- findConcernedFiles path opts
            print concerns
        [] -> panic "Usage: src-watch [-i ignores] path"
        _ -> panic "Extra rubbish on end of arguments, expected exactly one path"

panic :: String -> IO a
panic s = do
    hPutStrLn stderr s
    exitFailure
