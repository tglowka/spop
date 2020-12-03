module PositionsModule where

{-
  Konwertuje tablicę znaków na liczbę całkowitą
  Przykład: toInt ['5'] -> 5
-}
toInt :: [Char] -> Int
toInt x = read x :: Int

{-
  Zwraca tablicę pozycji cyfr w danym rzędzie. Na pierwszym miejscu pozycji jest indeks (liczony od 0), a na drugim cyfra.
  Przykład: getRowPositions "..5....54." -> [(2,5),(7,5),(8,4)]
-}
getRowPositions :: String -> [(Int, Int)]
getRowPositions [] = []
getRowPositions (x : xs) = getPositions (x : xs) [] 0
  where
    getPositions :: String -> [(Int, Int)] -> Int -> [(Int, Int)]
    getPositions [] positions _ = positions
    getPositions (x : xs) positions counter
      | x == '.' = getPositions xs positions (counter + 1)
      | otherwise = getPositions xs (positions ++ [(counter, toInt [x])]) (counter + 1)

{-
  Zwraca tablicę pozycji cyfr wraz z wartościami we wszystkich rzędach.
  Jeden element składa się krotki, która składa się z krotki ze współrzędnymi cyfry oraz wartości tj cyfry.
  Przykład: getBoardPositions ["..5....54.",".5..6..5.."] -> [((0,2),5),((0,7),5),((0,8),4),((1,1),5),((1,4),6),((1,7),5)]
-}
getBoardPositions :: [String] -> [((Int, Int), Int)]
getBoardPositions [] = []
getBoardPositions (x : xs) = func (x : xs) 0
  where
    func :: [String] -> Int -> [((Int, Int), Int)]
    func [] _ = []
    func (x : xs) rowIdx = [((rowIdx, fst y), snd y) | y <- getRowPositions x] ++ func xs (rowIdx + 1)

{-
  Zwraca mozliwych sąsiadów w zależności od położenia punktu przekazanego jako argument.
  (x,y) - współżędne punktu do sprawdzenia
  maxX - największy indeks na osi x
  maxY - największy indeks na osi y
-}
getPossibleNeghbours :: (Int, Int) -> Int -> Int -> [Int]
getPossibleNeghbours (0, 0) _ _ = [5, 6, 8, 9] --lewy gorny rog
getPossibleNeghbours (x, y) maxX maxY
  | x > 0 && x < maxX && y == 0 = [4, 5, 6, 7, 8, 9] --górny pasek
  | x == maxX && y == 0 = [4, 5, 7, 8] --prawy gorny rog
  | x == 0 && y == maxY = [2, 3, 5, 6] --lewy dolny rog
  | x > 0 && x < maxX && y == maxY = [1, 2, 3, 4, 5, 6] --dolny pasek
  | x == maxX && y == maxY = [1, 2, 4, 5] --prawy dolny rog
  | x == 0 && y > 0 && y < maxY = [2, 3, 5, 6, 8, 9] --lewy pionowy pasek
  | x == maxX && y > 0 && y < maxY = [1, 2, 4, 5, 7, 8] --prawy pionowy pasek
  | otherwise = [1, 2, 3, 4, 5, 6, 7, 8, 9] -- wszystko w srodku

getNeighbourPosition :: (Int, Int) -> Int -> (Int, Int)
getNeighbourPosition (x, y) code
  | code == 1 = (x -1, y -1)
  | code == 2 = (x, y -1)
  | code == 3 = (x + 1, y + 1)
  | code == 4 = (x -1, y)
  | code == 5 = (x, y)
  | code == 6 = (x + 1, y)
  | code == 7 = (x -1, y + 1)
  | code == 8 = (x, y + 1)
  | code == 9 = (x + 1, y + 1)

getAllCombinations :: Int -> [a] -> [[a]]
getAllCombinations 0 _ = [[]]
getAllCombinations _ [] = []
getAllCombinations n (x : xs) = (map (x :) (getAllCombinations (n -1) xs)) ++ (getAllCombinations n xs)

-- randomMark :: [String] -> [String]
-- randomMark (x:xs) (y:ys) =