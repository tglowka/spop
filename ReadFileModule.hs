module ReadFileModule where

{-
  Wczytuje łamigłówkę z pliku o podanej nazwie
  Przykład: readPuzzle "puzzle.txt"
-}

readPuzzle :: String -> IO [String]
readPuzzle filename = do
  contents <- readFile filename -- odczytaj całą zawartość pliku
  let puzzle = read contents :: [String] -- utwórz listę napisów (zob. klasa typów Read)
  return puzzle
