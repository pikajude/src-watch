import Options
import System.Directory
import System.Exit
import System.IO
import Watch

main :: IO ()
main = runCommand $ \ opts args ->
    case args of
        [dir] -> watch dir opts
        [] -> panic "Usage: src-watch [-i ignores] path"
        _ -> panic "Extra rubbish on end of arguments, expected exactly one path"

panic :: String -> IO a
panic s = do
    hPutStrLn stderr s
    exitFailure
