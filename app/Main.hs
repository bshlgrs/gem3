module Main where

import Gem3.Lib

main = case runProgram initialProgram of
  Left x -> putStrLn x
  Right x -> print x

