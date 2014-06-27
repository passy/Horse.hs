{-# LANGUAGE OverloadedStrings #-}
module Horse.MarkovChain (run) where

import Data.List
import System.Environment
import qualified Data.Map as Map
import Control.Monad.State
import System.Random

type MarkovMap = Map.Map String String
type MarkovState = (StdGen, String)

transition :: MarkovMap -> State MarkovState Char
transition m = do
    (gen, sofar) <- get
    let options = m Map.! sofar
        (index, newGen) = randomR (0, length options - 1) gen
        next = options !! index
    put (newGen, tail sofar ++ [next])
    return next

getWords :: MarkovMap -> Int -> [String]
getWords m n =
    let keys        = filter ((==) ' ' . last) $ Map.keys m
        (r, gen)    = randomR (0, length keys - 1) $ mkStdGen 137
        startState  = keys !! r
        markovChars = evalState (sequence . repeat $ transition m) (gen, startState)
    in  take n . words $ markovChars

chain :: Int -> String -> Map.Map String String
chain n xs =
    let from = map (take n) . tails $ xs ++ " " ++ xs
        to   = drop n . map (:[]) $ xs ++ " " ++ take n xs
    in Map.fromListWith (++) $ zip from to

run :: String -> Int -> Int -> String
run input n w =
    let chainMap = chain n . unwords . words $ input
    in unwords $ getWords chainMap w
