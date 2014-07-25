module Watch.Options where

import Control.Applicative
import Options

type Ignore = String

data WatchOptions = WatchOptions
                  { wIgnores :: [Ignore]
                  , wBasedir :: FilePath
                  , wDefines :: [(String, String)]
                  } deriving Show

instance Options WatchOptions where
    defineOptions = WatchOptions
        <$> defineOption optionType_strings
            (\ o -> o { optionShortFlags = "i"
                      , optionLongFlags = ["ignore"]
                      , optionDefault = []
                      , optionDescription =
                            "Filepaths to ignore. This option can be used multiple times."
                      })

        <*> defineOption optionType_string
            (\ o -> o { optionShortFlags = "d"
                      , optionLongFlags = ["base-dir"]
                      , optionDescription = "Working directory to assume."
                      })

        <*> defineOption optionType_cppdefine
            (\ o -> o { optionShortFlags = "D"
                      , optionLongFlags = ["cpp-define"]
                      , optionDescription = "CPP defines."
                      })

optionType_strings :: OptionType [String]
optionType_strings = (optionType "text" [] (Right . return) (const "none"))
    { optionTypeMerge = Just concat }

optionType_cppdefine :: OptionType [(String, String)]
optionType_cppdefine = (optionType "define" [] parseCPP (const "none"))
    { optionTypeMerge = Just concat }
    where
        parseCPP s = case span (/= '=') s of
            (a, b) -> Right [(a, drop 1 b)]

{-# ANN module "HLint: ignore Use camelCase" #-}
