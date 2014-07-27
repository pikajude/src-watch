import Watch
import Watch.Options

main :: IO ()
main = execParser optsI >>= \ args -> watch (wTarget args) args
    where
        optsI = info (helper <*> opts)
                (fullDesc
              <> progDesc "Watch a source directory for changes"
              <> header "src-watch" )
