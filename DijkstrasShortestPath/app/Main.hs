module Main (main) where

import Lib

main :: IO ()
main =  do
  putStrLn "Enter a number: "
  number <- getLine
  let result = factorial (read number :: Int)
  putStrLn ("The factorial of " ++ number ++ " = " ++ show result)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)