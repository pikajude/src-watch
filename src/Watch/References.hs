{-# LANGUAGE NoMonomorphismRestriction #-}

module Watch.References where

import Control.Lens
import Control.Monad
import Data.Data
import Data.Data.Lens
import Language.Haskell.Exts

filterADT :: (Data a, Data b) => a -> [b]
filterADT = universeOnOf biplate uniplate

splices :: Data a => a -> [String]
splices = concatMap fromExp
            . filter (\ x -> case x of SpliceExp{} -> True; _ -> False)
            . filterADT

fromExp = map (\ ~(Lit (String s)) -> s)
        . filter (\ x -> case x of Lit (String _) -> True; _ -> False)
        . filterADT
