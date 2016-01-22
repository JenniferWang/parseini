-- | Run ParseIni on input file, pretty printing the results
module Main where

import ParseIni
import PrettyPrintIni

import qualified Data.ByteString as B
import System.Exit

-- | Main - parse input file, then pretty-print the result
main :: IO ()
main = do
    content <- B.getContents
    let result = parseIniFile content
    either (\err -> putStrLn err >> exitFailure)
           (\success -> (B.putStr $ prettyPrint success) >> exitSuccess)
           result

-- main :: IO ()
-- main = do
--     content <- B.getContents
--     let result = main_test content
--     either (\err -> putStrLn err >> exitFailure)
--            (\success -> ((putStrLn . show) success) >> exitSuccess)
--            result


