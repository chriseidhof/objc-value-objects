{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs.Implicit
import System.IO (stdin)
import qualified Data.Text.IO as TIO
import qualified Compile as C
import Parser
import PPObjCSyntax
import System.FilePath (takeExtension)

data Options = Options {input :: String, output :: String} deriving (Show, Data, Typeable)

sample = Options { input = def &= argPos 0 &= typ "INPUT"
                 , output = def }
         &= summary "Objective-C value class generator 0.1"
main = process =<< cmdArgs sample

process (Options input output) = do
   contents <- TIO.readFile input
   case parse contents of
      Left err -> error err
      Right ast -> putStrLn $ show $ compile ast output

compile ast output = if (takeExtension output == ".h") 
                          then interface (C.interface ast)
                          else implementation (C.implementation ast)
