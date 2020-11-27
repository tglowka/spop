module MainModule where

import PositionsModule
import ReadFileModule

main = do
  putStr "Enter file name: "
  fileName <- getLine
  puzzle <- readPuzzle fileName
  putStrLn (show $ length $ puzzle)
