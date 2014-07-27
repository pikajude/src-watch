{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Watch.Options (
    WatchOptions (..), opts,
    module Options.Applicative
) where

import Control.Applicative
import Control.Arrow
import Options.Applicative
import Watch.Logger

type Ignore = String

data WatchOptions = WatchOptions
                  { wTarget :: FilePath
                  , wIgnores :: [Ignore]
                  , wDefines :: [(String, String)]
                  , wLevel :: Verbosity
                  } deriving Show

opts = WatchOptions
   <$> argument Just (metavar "ROOT")
   <*> many (strOption
                (short 'i'
              <> long "ignore"
              <> metavar "PATTERN"
              <> help "Ignore a filepath. This follows gitignore syntax."))
   <*> many (cppOption
                (short 'D'
              <> long "cpp-define"
              <> help "Define a symbol when running cpphs on source files."))
   <*> fmap toEnumC
       (option (short 'v'
                <> value 0
                <> help "Verbosity (0..1). Omitting this option is equivalent to -v0."))

toEnumC :: forall a. (Enum a, Bounded a) => Int -> a
toEnumC x | x >= fromEnum (maxBound :: a) = maxBound
          | x <= fromEnum (minBound :: a) = minBound
          | otherwise = toEnum x

cppOption s = nullOption (s <> reader cpp) where
    cpp s = return (second (drop 1) $ span (/= '=') s)

clamp :: Ord a => (a, a) -> a -> a
clamp (x, y) z
    | z <= x = x
    | z >= y = y
    | otherwise = z
