module PositionsModule where

{-
  Konwertuje tablicę znaków na liczbę całkowitą
  Przykład: toInt ['5'] -> 5
-}
toInt :: [Char] -> Int
toInt x = read x :: Int

{-
  Zwraca listę pozycji cyfr w danym rzędzie. Na pierwszym miejscu pozycji jest indeks (liczony od 0), a na drugim cyfra.
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
  Zwraca listę pozycji cyfr wraz z wartościami we wszystkich rzędach.
  Jeden element składa się krotki, która składa się z krotki ze współrzędnymi cyfry oraz wartości tj cyfry.
  Przykład: getBoardPositions ["..5....54.",".5..6..5.."] -> [((0,2),5),((0,7),5),((0,8),4),((1,1),5),((1,4),6),((1,7),5)]
  Argumenty:
  - lista tablic znaków (wczytana plansza)
  Return: Lista pozycji cyfr wraz z wartościami we wszystkich rzędach.
-}
getBoardPositions :: [String] -> [((Int, Int), Int)]
getBoardPositions [] = []
getBoardPositions (x : xs) = func (x : xs) 0
  where
    func :: [String] -> Int -> [((Int, Int), Int)]
    func [] _ = []
    func (x : xs) rowIdx =
      [ ((fst y, rowIdx), snd y)
        | y <- getRowPositions x
      ]
        ++ func xs (rowIdx + 1)

{-
  Zwraca możliwych sąsiadów w zależności od położenia punktu przekazanego jako argument.
  Argumenty:
  - maxX - największy indeks na osi x
  - maxY - największy indeks na osi y
  - (x,y) - współżędne punktu do sprawdzenia
-}
getPossibleNeighboursPositions :: Int -> Int -> (Int, Int) -> [(Int, Int)]
getPossibleNeighboursPositions maxX maxY (x, y)
  | x == 0 && y == 0 = [p5, p6, p8, p9] --lewy gorny rog
  | x > 0 && x < maxX && y == 0 = [p4, p5, p6, p7, p8, p9] --górny pasek
  | x == maxX && y == 0 = [p4, p5, p7, p8] --prawy gorny rog
  | x == 0 && y == maxY = [p2, p3, p5, p6] --lewy dolny rog
  | x > 0 && x < maxX && y == maxY = [p1, p2, p3, p4, p5, p6] --dolny pasek
  | x == maxX && y == maxY = [p1, p2, p4, p5] --prawy dolny rog
  | x == 0 && y > 0 && y < maxY = [p2, p3, p5, p6, p8, p9] --lewy pionowy pasek
  | x == maxX && y > 0 && y < maxY = [p1, p2, p4, p5, p7, p8] --prawy pionowy pasek
  | otherwise = [p1, p2, p3, p4, p5, p6, p7, p8, p9] -- wszystko w srodku
  where
    p1 = (x -1, y -1)
    p2 = (x, y -1)
    p3 = (x + 1, y - 1)
    p4 = (x -1, y)
    p5 = (x, y)
    p6 = (x + 1, y)
    p7 = (x -1, y + 1)
    p8 = (x, y + 1)
    p9 = (x + 1, y + 1)

{-
  Zwraca listę punktów tzn. obecnie zamalowanych sąsiadów
  Argumenty:
  - maxX - największy indeks na osi x
  - maxY - największy indeks na osi y
  - współrzędne punktu, dla którego chcemy otrzymać aktualnie zamalowanych sąsiadów
  - lista obecnie zamalowanych punktów
  Return: Lista punktów obecnie zamalowanych sąsiadów
-}

getCurrentNeighboursPositions :: Int -> Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getCurrentNeighboursPositions _ _ _ [] = []
getCurrentNeighboursPositions maxX maxY (x, y) markedPoints =
  [ x1
    | x1 <- markedPoints, -- lista punków obecnie zamalowanych
      y1 <- getPossibleNeighboursPositions maxX maxY (x, y), -- lista wszystkich możliwych sąsiadów w zależności od przekazanego punktu
      x1 == y1 -- bierzemy te punkty, które są identyczne z obu list
  ]

{-
  Zwraca listę wszystkich możliwych kombinacji bez powtórzeń przekazanego zbioru
  Argumenty:
  - rozmiar podzbioru
  - zbiór, z którego tworzone są kombinacje bez powtórzeń
  Return: Lista list (podzbiorów)
-}
getAllCombinations :: Int -> [a] -> [[a]]
getAllCombinations 0 _ = [[]]
getAllCombinations _ [] = []
getAllCombinations n (x : xs) = (map (x :) (getAllCombinations (n -1) xs)) ++ (getAllCombinations n xs)

{-
  Zwraca liste punktów, które należy sprawdzić w zależności od przekazanego punktu.
  Argumenty:
  - współrzędne punktu, dla którego chcemy sprawdzić punkty do sprawdzenia
  Return: Lista punktów, które należy sprawdzić w zależności od przekazanego punktu.
-}
getPointsToCheck :: (Int, Int) -> [(Int, Int)]
getPointsToCheck (x, y) = [(x1, y1) | x1 <- [x -2 .. x + 2], y1 <- [y -2 .. y], (x1, y1) /= (x, y), (x1, y1) /= (x + 1, y), (x1, y1) /= (x + 2, y), x1 >= 0, y1 >= 0]