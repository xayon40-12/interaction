module Value where

import Data.List.Utils
import Data.List

type State = [Int]
type Possibilities = [State]

type Law = (State,State)
type Laws = [Law]

apply :: Law -> State -> Maybe State
apply (i,f) s = go i f s
    where
        go (i:is) (f:fs) (s:ss) = (:) <$> try i f s <*> go is fs ss
        go (i:is) [] (s:ss) = (:) <$> try i 0 s <*> go is [] ss
        go (_:_) _ [] = Nothing
        go [] (f:fs) (s:ss) = (:) <$> Just (f+s) <*> go [] fs ss
        go [] fs [] = Just fs
        go [] [] ss = Just ss
        try i f s = if i<=s then Just (s-i+f) else Nothing
        
evolve :: Laws -> Possibilities -> Possibilities
evolve ls ss = sort . uniq $ go ls =<< ss
    where
        go [] _ = []
        go (l:ls) sys = case apply l sys of
            Just s -> s:go ls sys
            Nothing -> go ls sys

pretty :: Possibilities -> String
pretty [] = ""
pretty [p] = "|" ++ (((++"|").show) =<< p)
pretty (p:ps) = "|" ++ (((++"|").show) =<< p) ++ " - " ++ pretty ps
