module Main where

import PositionsModule
import ReadFileModule

{-
  Argumenty:
    - maksymalny indeks X planszy
    - maksymalny indeks Y planszy
    - lista punktów do przerobienia(krotek), krotka taka że = ((x,y),z):
      - x - indeks x punktu
      - y - indeks y punktu
      - z - wartość punktu
    - lista przerobionych punktów
    - lista obecnie zamalowanych punktów
  Return: lista końcowa zamalowanych punktów
-}

processBoardPosition :: Int -> Int -> [((Int, Int), Int)] -> [((Int, Int), Int)] -> [(Int, Int)] -> [(Int, Int)]
processBoardPosition maxX maxY [] processedPoints markedPoints = markedPoints
processBoardPosition maxX maxY (point : restPoints) processedPoints markedPoints =
  isOK maxX maxY point us processedPoints markedPoints
  where
    pointCoordiantes = fst point
    poitnValue = snd point
    currentNeighbours = getCurrentNeighboursPositions maxX maxY pointCoordiantes markedPoints
    currentNeighboursCount = length currentNeighbours
    possibleNeighbours = getPossibleNeighboursPositions maxX maxY pointCoordiantes
    missingNeighboursCount = poitnValue - currentNeighboursCount
    unusedNeighbours = [x | x <- possibleNeighbours, x `notElem` currentNeighbours]
    us = getAllCombinations missingNeighboursCount unusedNeighbours
    isOK :: Int -> Int -> ((Int, Int), Int) -> [[(Int, Int)]] -> [((Int, Int), Int)] -> [(Int, Int)] -> [(Int, Int)]
    isOK maxX maxY point [] processedPoints markedPoints = [(-1, -1)]
    isOK maxX maxY point (cmb : cmbs) processedPoints markedPoints
      | result == [(-1, -1)] = isOK maxX maxY point cmbs processedPoints markedPoints
      | otherwise = evalNext processNext
      where
        result = markAndCheckNeighbours maxX maxY point cmb processedPoints markedPoints
        processNext = processBoardPosition maxX maxY restPoints ([point] ++ processedPoints) result
        evalNext resultNext
          | resultNext == [(-1, -1)] = isOK maxX maxY point cmbs processedPoints markedPoints
          | otherwise = resultNext

{-
  Argumenty:
  - maksymalny indeks X planszy
  - maksymalny indeks Y planszy
  - współrzędne punktu wraz z wartością
  - lista punktów do zamalowania
  - lista dotychczas przerobionych punktów wraz z wartościami
  - lista dotychczas zamalowanych pól
  Return:
    Jeśli zamalowane punkty nie zepsuły dotychczas przerobionych pól - Lista dotychczas zamalowanych pól + nowo zamalowane punkty
    Jeśli zamalowane punkty zepsuły jedno z dotychczas przerobionych punktów - [(-1,-1)]
-}
markAndCheckNeighbours :: Int -> Int -> ((Int, Int), Int) -> [(Int, Int)] -> [((Int, Int), Int)] -> [(Int, Int)] -> [(Int, Int)]
markAndCheckNeighbours maxX maxY ((x, y), z) [] processedPoints markedPoints = markedPoints
markAndCheckNeighbours maxX maxY ((x, y), z) newMarkedPoints [] [] = newMarkedPoints
markAndCheckNeighbours maxX maxY ((x, y), z) newMarkedPoints processedPoints markedPoints
  | result = newMarkedPoints ++ markedPoints
  | otherwise = [(-1, -1)]
  where
    pointsToCheck =
      [ x1
        | x1 <- processedPoints,
          y1 <- getPointsToCheck (x, y),
          fst x1 == y1
      ]
    validatePointWithNewMarked :: [((Int, Int), Int)] -> [(Int, Int)] -> Bool
    validatePointWithNewMarked [] _ = True
    validatePointWithNewMarked (point : points) markedPoints
      | pointValue < currentNeighboursCount = False
      | otherwise = validatePointWithNewMarked points markedPoints
      where
        pointCoordiantes = fst point
        pointValue = snd point
        currentNeighboursCount = length $ getCurrentNeighboursPositions maxX maxY pointCoordiantes markedPoints
    result = validatePointWithNewMarked pointsToCheck (newMarkedPoints ++ markedPoints)

-- quicksort :: [(Int, Int)] -> [(Int, Int)]
-- quicksort [] = []
-- quicksort (x : xs) = quicksort smaller ++ x : quicksort larger
--   where
--     smaller = [y | y <- xs, (fst y <= fst x) && (snd y == snd x) || (snd y < snd x)]
--     larger = [y | y <- xs, (fst y > fst x) && (snd y == snd x) || (snd y > snd x)]

getFilledBoard :: Int -> Int -> [(Int, Int)] -> [String]
getFilledBoard maxX maxY (x : xs) =
  getBoard 0 0 (x : xs) [] []
  where
    getBoard :: Int -> Int -> [(Int, Int)] -> String -> [String] -> [String]
    getBoard counterX counterY [] agg result = result
    getBoard counterX counterY points agg result
      | counterY > maxY = result
      | (counterX, counterY) `elem` points && counterX == maxX = getBoard 0 (counterY + 1) points [] (result ++ [agg ++ ['x']])
      | (counterX, counterY) `elem` points && counterX < maxX = getBoard (counterX + 1) counterY points (agg ++ ['x']) result
      | (counterX, counterY) `notElem` points && counterX == maxX = getBoard 0 (counterY + 1) points [] (result ++ [agg ++ ['.']])
      | (counterX, counterY) `notElem` points && counterX < maxX = getBoard (counterX + 1) counterY points (agg ++ ['.']) result

printFilledBoard :: [String] -> IO ()
printFilledBoard [] = return ()
printFilledBoard (x : xs) =
  do
    putStrLn x
    printFilledBoard xs

main :: IO ()
main = do
  putStr "Enter file name: "
  fileName <- getLine
  puzzle <- readPuzzle fileName
  let maxX = (length $ head puzzle) -1
  let maxY = (length puzzle) -1
  let boardPositions = getBoardPositions puzzle
  let final = processBoardPosition maxX maxY boardPositions [] []
  let filledBoard = getFilledBoard maxX maxY final
  print final
  printFilledBoard filledBoard
  putStrLn (show $ length $ puzzle)