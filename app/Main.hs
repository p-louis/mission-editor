module Main where

import Lib
import System.Environment
import System.Exit

main :: IO ()
main = do
    getArgs >>= parseArgs

parseArgs fs = parse $ concat fs


