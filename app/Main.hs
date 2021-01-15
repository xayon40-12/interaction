module Main where

import Value

main :: IO ()
main = do
    let steps = iterate (evolve [([1],[0,1]),([0,1],[0,0,1]),([1,0,1],[0,2]),([0,0,1],[1]),([0,1],[0,0,0,1])]) [[2]]
    mapM_ (putStrLn . pretty) $ take 40 steps
