-- https://stackoverflow.com/questions/7867723/haskell-file-reading
-- https://stackoverflow.com/questions/36502375/haskell-how-to-print-each-element-of-list-separated-with-comma
-- https://www.reddit.com/r/haskell/comments/8jui5k/how_to_replace_an_element_at_an_index_in_a_list/


--- Imports
import System.IO  
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef


--- Custom Types
data Mapstate = Revealed | Hidden | Flagged
instance Eq Mapstate where
  Revealed == Revealed = True
  Hidden == Hidden = True
  Flagged == Flagged = True
  _ == _ = False

data Gamestate = Playing | Won | Lost
instance Eq Gamestate where
  Playing == Playing = True
  Won == Won = True
  Lost == Lost = True
  _ == _ = False


--- Global variables
winmsg = "You won, splendid job."
losemsg = "You lost, better luck next time :c"


--- Main functions
-- Starting function
main :: IO ()
main = do
  -- Query for map file
  putStrLn "> Which file should I read from?"
  filename <- getLine
  
  -- Read map file
  putStrLn "> Reading from file: ..."
  contents <- readFile filename
  
  -- Parse map and generate mask
  let basemap = map words . lines $ contents
  let mask = maskMap basemap

  -- Check if map is valid
  if isMapValid basemap then do
    let mine_count = countMines basemap

    -- Invoke game loop
    inputLoop basemap (mask, mine_count, Playing)
  else do putStrLn "Invalid map"

-- Main loop
inputLoop :: [[String]] -> ([[Mapstate]], Int, Gamestate) -> IO ()
inputLoop gamemap (_, _, Won) = do
  putStrLn winmsg
  indexedPrintLists gamemap
inputLoop gamemap (_, _, Lost) = do
  putStrLn losemsg
  indexedPrintLists gamemap
inputLoop gamemap (mask, am_flags, gamestate) = do
  -- Generates and display map
  let dmap = displayMap gamemap mask
  indexedPrintLists dmap

  putStrLn ("   => " ++ (show am_flags) ++ " flags left")
  -- Query for action
  putStr "> Please input action: " 
  mInput <- getLine

  -- Split input in command and args 
  let com = head . words $ mInput
  let coords = tail . words $ mInput
  
  case com of
    "q"       -> do return ()
    "f"       -> do inputLoop gamemap (flag gamemap (mask, am_flags, gamestate) (read (coords !! 0), read (coords !! 1)))
    "u"       -> do inputLoop gamemap (unflag gamemap (mask, am_flags, gamestate) (read (coords !! 0), read (coords !! 1)))
    "r"       -> do inputLoop gamemap (reveal gamemap (mask, am_flags, gamestate) (read (coords !! 0), read (coords !! 1)))
    "quit"    -> do return ()
    "flag"    -> do inputLoop gamemap (flag gamemap (mask, am_flags, gamestate) (read (coords !! 0), read (coords !! 1)))
    "unflag"  -> do inputLoop gamemap (unflag gamemap (mask, am_flags, gamestate) (read (coords !! 0), read (coords !! 1)))
    "reveal"  -> do inputLoop gamemap (reveal gamemap (mask, am_flags, gamestate) (read (coords !! 0), read (coords !! 1)))
    "help"    -> do putStrLn "available commands: flag col row, unflag col row, reveal col row, q" >> inputLoop gamemap (mask, am_flags, gamestate)
    otherwise -> do putStrLn "Please enter a valid action" >> inputLoop gamemap (mask, am_flags, gamestate)


--- User actions
-- Reveals cell and updates gamestate
reveal :: [[String]] -> ([[Mapstate]], Int, Gamestate) -> (Int, Int) -> ([[Mapstate]], Int, Gamestate)
reveal gamemap (mask, am_flags, gamestate) (x,y)
  | y < 0 || x < 0 || y >= length gamemap || x >= length (gamemap !! 0) = (mask, am_flags, gamestate)
  | ((mask !! y) !! x) /= Hidden = (mask, am_flags, gamestate)
  | isMine
 ((gamemap !! y) !! x) = (replace mask (y,x,Revealed), am_flags, Lost)
  | ((gamemap !! y) !! x) /= "0" = do
    let new_mask = replace mask (y,x,Revealed)
    if isMapComplete gamemap new_mask then do 
      (new_mask, am_flags, Won)
    else do
      (new_mask, am_flags, gamestate)
  | otherwise = do
      let lmask = (replace mask (y,x,Revealed), am_flags, gamestate)
      let lmask1 = reveal gamemap lmask (x-1, y-1)
      let lmask2 = reveal gamemap lmask1 (x-1, y)
      let lmask3 = reveal gamemap lmask2 (x-1, y+1)
      let lmask4 = reveal gamemap lmask3 (x, y-1)
      let lmask5 = reveal gamemap lmask4 (x, y+1)
      let lmask6 = reveal gamemap lmask5 (x+1, y-1)
      let lmask7 = reveal gamemap lmask6 (x+1, y)
      let lmask8 = reveal gamemap lmask7 (x+1, y+1)

      let new_mask = getFirst lmask8
      if isMapComplete gamemap new_mask then do 
        (new_mask, am_flags, Won)
      else do
        (new_mask, am_flags, gamestate)

-- Flags cell
flag :: [[String]] -> ([[Mapstate]], Int, Gamestate) -> (Int, Int) -> ([[Mapstate]], Int, Gamestate)
flag gamemap (mask, am_flags, gamestate) (x,y)
  | y < 0 || x < 0 || y >= length gamemap || x >= length (gamemap !! 0) = (mask, am_flags, gamestate)
  | am_flags == 0 = (mask, am_flags, gamestate)
  | ((mask !! y) !! x) /= Hidden = (mask, am_flags, gamestate)
  | otherwise = do
    (replace mask (y,x,Flagged), am_flags-1, gamestate)

-- Unflags cell
unflag :: [[String]] -> ([[Mapstate]], Int, Gamestate) -> (Int, Int) -> ([[Mapstate]], Int, Gamestate)
unflag gamemap (mask, am_flags, gamestate) (x,y)
  | y < 0 || x < 0 || y >= length gamemap || x >= length (gamemap !! 0) = (mask, am_flags, gamestate)
  | ((mask !! y) !! x) /= Flagged = (mask, am_flags, gamestate)
  | otherwise = do
    (replace mask (y,x,Hidden), am_flags+1, gamestate)


--- Main game logic
-- Check if map is valid
isMapValid :: [[String]] -> Bool
isMapValid gamemap = _x_isMapValid gamemap (gamemap, 0)

_x_isMapValid :: [[String]] -> ([[String]], Int) -> Bool
_x_isMapValid [x] (gamemap, i) = (_y_isMapValid x (gamemap, i, 0))
_x_isMapValid (x:xs) (gamemap, i) = (_y_isMapValid x (gamemap, i, 0)) && (_x_isMapValid xs (gamemap, i+1))

_y_isMapValid :: [String] -> ([[String]], Int, Int) -> Bool
_y_isMapValid [x] (gamemap, i, j) = (isTileValid gamemap (i, j))
_y_isMapValid (x:xs) (gamemap, i, j) = (isTileValid gamemap (i, j)) && (_y_isMapValid xs (gamemap, i, j+1))

isTileValid :: [[String]] -> (Int, Int) -> Bool
isTileValid gamemap (x,y)
  | isMine
 ((gamemap !! y) !! x) = True
  | otherwise = do
    let amine = readInt ((gamemap !! y) !! x)
    let amine1 = amine - (fromEnum (isTileBomb gamemap (x-1, y-1)))
    let amine2 = amine1 - (fromEnum (isTileBomb gamemap (x-1, y)))
    let amine3 = amine2 - (fromEnum (isTileBomb gamemap (x-1, y+1)))
    let amine4 = amine3 - (fromEnum (isTileBomb gamemap (x, y-1)))
    let amine5 = amine4 - (fromEnum (isTileBomb gamemap (x, y+1)))
    let amine6 = amine5 - (fromEnum (isTileBomb gamemap (x+1, y-1)))
    let amine7 = amine6 - (fromEnum (isTileBomb gamemap (x+1, y)))
    let amine8 = amine7 - (fromEnum (isTileBomb gamemap (x+1, y+1)))

    amine8 == 0

isTileBomb :: [[String]] -> (Int, Int) -> Bool
isTileBomb gamemap (x,y)
  | y < 0 || x < 0 || y >= length gamemap || x >= length (gamemap !! 0) = False
  | otherwise = isMine
 ((gamemap !! y) !! x)

-- Check if game is complete (won)
isMapComplete :: [[String]] -> [[Mapstate]] -> Bool
isMapComplete gamemap mask = _x_isMapComplete gamemap (gamemap, mask, 0)

_x_isMapComplete :: [[String]] -> ([[String]], [[Mapstate]], Int) -> Bool
_x_isMapComplete [x] (gamemap,mask,i) = (_y_isMapComplete x (gamemap, mask, i, 0))
_x_isMapComplete (x:xs) (gamemap,mask,i) = (_y_isMapComplete x (gamemap, mask, i, 0)) && (_x_isMapComplete xs (gamemap, mask, i+1))

_y_isMapComplete :: [String] -> ([[String]], [[Mapstate]], Int, Int) -> Bool
_y_isMapComplete [x] (gamemap,mask,i,j) = (isTileComplete gamemap mask (i, j))
_y_isMapComplete (x:xs) (gamemap,mask,i,j) = (isTileComplete gamemap mask (i, j)) && (_y_isMapComplete xs (gamemap, mask, i, j+1))

isTileComplete :: [[String]] -> [[Mapstate]] -> (Int,Int) -> Bool
isTileComplete gamemap mask (x,y)
  | y < 0 || x < 0 || y >= length gamemap || x >= length (gamemap !! 0) = True
  | isMine
 ((gamemap !! y) !! x) = True
  | otherwise = ((mask !! y) !! x) == Revealed


--- Helper functions
-- Checks if input is mine
isMine :: String -> Bool
isMine "M" = True
isMine _ = False

-- Counts mines present in game map 
countMines :: [[String]] -> Int
countMines [] = 0
countMines [x] = _countMines x
countMines (x:xs) = _countMines x + countMines xs

_countMines :: [String] -> Int
_countMines [] = 0
_countMines [x] = fromEnum (isMine x)
_countMines (x:xs) = fromEnum (isMine x) + _countMines xs

-- Print list to console
printLists :: [[String]] -> IO ()
printLists [] = return ()
printLists [x] = printList x
printLists (x:xs) = do 
  printList x 
  printLists xs

printList :: [String] -> IO ()
printList [] = return ()
printList [x] = putStrLn x
printList (x:xs) = do
  putStr x
  if (length x) == 1 then do putStr "  "
  else do putStr " "
  printList xs

-- Print list with indexing
indexedPrintLists :: [[String]] -> IO ()
indexedPrintLists [] = return ()
indexedPrintLists [x] = printList x
indexedPrintLists (x:xs) = do
  putStr "     "
  printList . map show $ [0..((length (x:xs))-1)]
  putStrLn ("    " ++ generateLine (length (x:xs)))
  _indexedPrintLists (x:xs) 0

_indexedPrintLists :: [[String]] -> Int -> IO ()
_indexedPrintLists [] _ = return ()
_indexedPrintLists [x] i = indexedprintList x i
_indexedPrintLists (x:xs) i = do
  indexedprintList x i
  _indexedPrintLists xs (i+1)

indexedprintList :: [String] -> Int -> IO ()
indexedprintList [] _ = return ()
indexedprintList [x] _ = putStrLn x
indexedprintList (x:xs) i = do
  putStr (show i)
  if i < 10 then do putStr "  | "
  else do putStr " | "
  printList (x:xs)

-- Parse string to int
readInt :: String -> Int
readInt = read

-- Generates line
generateLine :: Int -> String
generateLine 1 = "---"
generateLine i = "---" ++ generateLine (i-1)

-- Generates display map
displayMap :: [[String]] -> [[Mapstate]] -> [[String]]
displayMap [x] [y] = [_displayMap x y]
displayMap (x:xs) (y:ys) = [_displayMap x y] ++ (displayMap xs ys)

_displayMap :: [String] -> [Mapstate] -> [String]
_displayMap [x] [y]
  | y == Revealed = [x]
  | y == Hidden   = ["■"]
  | y == Flagged  = ["X"]
_displayMap (x:xs) (y:ys)
  | y == Revealed = [x] ++ (_displayMap xs ys)
  | y == Hidden   = ["■"] ++ (_displayMap xs ys)
  | y == Flagged  = ["X"] ++ (_displayMap xs ys)

-- Generate masked map we will modify
maskMap :: [[String]] -> [[Mapstate]]
maskMap [x] = [_maskMap x]
maskMap (x:xs) = [_maskMap x] ++ (maskMap xs)

_maskMap :: [String] -> [Mapstate]
_maskMap [x] = [Hidden]
_maskMap (x:xs) = [Hidden] ++ (_maskMap xs)

-- Replaces value of element at given index
replace [[]] _ = [[]]
replace (x:xs) (0,m,a) = (_replace x (m,a)):xs
replace (x:xs) (n,m,a) =
  if n < 0
    then (x:xs)
    else x: replace xs (n-1,m,a)

_replace [] _ = []
_replace (_:xs) (0,a) = a:xs
_replace (x:xs) (n,a) =
  if n < 0
    then (x:xs)
    else x: _replace xs (n-1,a)

-- Gets first element of tuple
getFirst (x, _, _) = x