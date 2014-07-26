{-# LANGUAGE NoMonomorphismRestriction #-}

module Watch.References where

import Control.Lens
import Data.Data
import Data.Data.Lens
import Data.Maybe
import Language.Haskell.Exts

filterADT :: (Data a, Data b) => (b -> Maybe c) -> a -> [c]
filterADT f = mapMaybe f . universeOnOf biplate uniplate

splices :: Data a => a -> [String]
splices = concatMap fromExp
            . filterADT (\ x -> case x of SpliceExp{} -> Just x; _ -> Nothing)

fromExp = filterADT (\ x -> case x of Lit (String s) -> Just s; _ -> Nothing)
