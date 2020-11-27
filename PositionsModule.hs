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
  Zwraca tablicę tablic pozycji cyfr we wszystkich rzędach. Na pierwszym miejscu pozycji jest indeks (liczony od 0), a na drugim cyfra.
  Przykład: getBoardPositions ["..5....54.",".5..6..5.."] -> [ [(2,5),(7,5),(8,4)] , [(1,5),(4,6),(7,5)] ]
-}
getBoardPositions :: [String] -> [[(Int, Int)]]
getBoardPositions [] = []
getBoardPositions (x : xs) = [getRowPositions x] ++ getBoardPositions (xs)