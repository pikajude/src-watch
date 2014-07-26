module Watch.Options where

import Control.Applicative
import Options
import Watch.Logger

type Ignore = String

data WatchOptions = WatchOptions
                  { wIgnores :: [Ignore]
                  , wDefines :: [(String, String)]
                  , wLevel :: Verbosity
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

        <*> defineOption optionType_cppdefine
            (\ o -> o { optionShortFlags = "D"
                      , optionLongFlags = ["cpp-define"]
                      , optionDescription = "CPP defines."
                      })

        <*> fmap (\ i -> reverse [minBound .. maxBound] !! clamp (0, 2) i)
            (defineOption optionType_verbosity
                (\ o -> o { optionShortFlags = "v"
                          , optionDescription =
                              "Verbosity. Repeat this flag to get more verbose."
                          }))

clamp :: Ord a => (a, a) -> a -> a
clamp (x, y) z
    | z <= x = x
    | z >= y = y
    | otherwise = z

optionType_strings :: OptionType [String]
optionType_strings = (optionType "text" [] (Right . return) (const "none"))
    { optionTypeMerge = Just concat }

optionType_cppdefine :: OptionType [(String, String)]
optionType_cppdefine = (optionType "define" [] parseCPP (const "none"))
    { optionTypeMerge = Just concat }
    where
        parseCPP s = case span (/= '=') s of
            (a, b) -> Right [(a, drop 1 b)]

optionType_verbosity :: OptionType Int
optionType_verbosity = (optionType "verbosity" 0 (const (Right 1)) (const "info"))
    { optionTypeMerge = Just length
    , optionTypeUnary = Just 1 }

{-# ANN module "HLint: ignore Use camelCase" #-}
