module Main where

import Options.Applicative
import System.Environment (getArgs)

data Option
  = Provide String
  | Produce

parser :: Parser Option
parser = do
  undefined

main :: IO ()
main = do
  return ()
