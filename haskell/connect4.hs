-- Connect 4
-- A piece will be represented as a 3-tuple of the x-position, y-position, and color. The colors are either 'X' or 'O', standing for red or yellow. (ex. (1, 2, 'r') is a red piece at the position 1, 2. (0, 0) will be the bottom leftmost position on the board.
-- The game will be represented as a list containing pieces. The maximum number of pieces on a board is 42 (7 columns, 6 rows). 

-- Some simple functions to get values from a piece
xPos :: (Int, b, c) -> Int
xPos (x, y, c) = x
yPos :: (a, Int, c) -> Int
yPos (x, y, c) = y
color :: (a, b, Char) -> Char
color (x, y, c) = c
pieceExists :: (Int, Int) -> [(Int, Int, Char)] -> Bool
pieceExists (x, y) board
  | (x, y, 'X') `elem` board || (x, y, 'O') `elem` board = True
  | otherwise = False

-- drawBoard takes a list containing pieces and returns a string that looks like the board
pieceToChar :: (Int, Int) -> [(Int, Int, Char)] -> Char
pieceToChar (x, y) board
  | (x, y, 'X') `elem` board = 'X'
  | (x, y, 'O') `elem` board = 'O'
  | x > 6 = '\n'
  | otherwise = '_'
drawBoard :: [(Int, Int, Char)] -> [Char]
drawBoard board = [pieceToChar (x, y) board | y <- [5, 4 .. 0], x <- [0 .. 7]]

-- makeMove takes a board, a player ('X' or 'O'), and a column number ([0 .. 6]) and puts a piece onto the board
movePiece :: (Int, Int, Char) -> [(Int, Int, Char)] -> (Int, Int, Char)
movePiece (x, y, c) board
  | y == 0 = (x, y, c)
  | pieceExists (x, y - 1) board = (x, y, c)
  | otherwise = movePiece (x, y - 1, c) board
makeMove :: [(Int, Int, Char)] -> Char -> Int -> [(Int, Int, Char)]
makeMove board player column = (movePiece (column, 8, player) board) : board

-- isLegalMove takes a board, a player ('X' or 'O'), and a column number ([0 .. 6]) and checks whether the move is legal
isLegalMove :: [(Int, Int, Char)] -> Int -> Bool
isLegalMove board column
  | (xPos piece > 6) || (xPos piece < 0) = False
  | (yPos piece > 5) || (yPos piece < 0) = False
  | otherwise = True
  where piece = movePiece (column, 8, 'X') board

-- isWon takes a board and checks whether either player has won
-- checkDir checks whether a single piece has won in a single direction
checkDirHelper board (a, b, c) (x, y) numToCheck
  | numToCheck == 0 = True
  | (a, b, c) `elem` board = checkDirHelper board (a + x, b + y, c) (x, y) (numToCheck - 1)
  | otherwise = False
checkDir :: [(Int, Int, Char)] -> (Int, Int, Char) -> (Int, Int) -> Bool
checkDir board piece dir = checkDirHelper board piece dir 4
dirList = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, -1)]
isWon :: [(Int, Int, Char)] -> Bool
isWon board = True `elem` [checkDir board piece dir | piece <- board, dir <- dirList]

-- anyLegal takes a player and a board and returns the first legal move it finds. If it can't find a legal move on the bord it returns (-99, -99, player)
anyLegal :: Char -> [(Int, Int, Char)] -> (Int, Int, Char)
anyLegal player board = anyLegalHelper player board 0
  where anyLegalHelper player board column
          | column > 6 = (-99, -99, player)
          | isLegalMove board column = movePiece (column, 8, player) board
          | otherwise = anyLegalHelper player board (column + 1)

-- winOrAnyMove takes a player and a board and returns a winning move if there is one or else it returns the first legal move
winOrAnyMove :: Char -> [(Int, Int, Char)] -> (Int, Int, Char)
winOrAnyMove player board = winOrAnyMoveHelper player board 0
  where winOrAnyMoveHelper player board column
          | column > 6 = anyLegal player board
          | isWon (makeMove board player column) = movePiece (column, 8, player) board
          | otherwise = winOrAnyMoveHelper player board (column + 1)

-- winOrGetThree takes a piece and tries to get three in a row or returns the first legal move
winOrGetThree :: Char -> [(Int, Int, Char)] -> (Int, Int, Char)
winOrGetThree player board = winOrGetThreeHelper player board 0
  where winOrGetThreeHelper player board column 
          | column > 6 = anyLegal player board
          | True `elem` [checkDirHelper board piece dir 3| piece <- (makeMove board player column), dir <- dirList] = movePiece (column, 8, player) board
          | otherwise = winOrGetThreeHelper player board (column + 1)

-- UI Code
winMessage board player = do
    putStrLn $ "The game is over"
    putStrLn $ "Player "++[(next player)]++" won!"
next player = if player == 'X' then 'O' else 'X'

main :: IO()
main = do
  let board = []
  eventLoop board 'X'

eventLoop :: [(Int, Int, Char)] -> Char -> IO()
eventLoop board player = do
  putStrLn $ drawBoard board
  if isWon board then do
    winMessage board player
    return ()
  else do
    col <- getMove player
    handleMove board player col

handleMove :: [(Int, Int, Char)] -> Char -> Int -> IO()
handleMove board player col
    | col == -99 = goodbye
    | isLegalMove board col = eventLoop newBoard (next player)
    | otherwise = complainAndRestart
    where complainAndRestart = do
              putStrLn "ERROR: That is not a legal move."
              eventLoop board player
          newBoard = makeMove board player col
          goodbye = do putStrLn "You quit"

getMove :: Char -> IO Int
getMove player = do
  putStrLn $ "(Enter -99 to quit.)"
  putStrLn $ "Player " ++(show player)++" moves."
  putStr $ "Column [0-6]? "
  x <- getLine
  return (getNumber x)

-- get_number returns -1 for any invalid input
getNumber :: String -> Int
getNumber colIn
    = case (reads colIn)::[(Int,String)] of
       [(colnum, "")] -> colnum
       _              -> -1



