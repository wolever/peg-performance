module Main (main) where

import Data.List (delete, concatMap, foldl')
import Data.Time.Clock.POSIX (getPOSIXTime)

data Coordinate = Coordinate {row :: Int, hole :: Int} deriving (Eq)

makeCoord :: Int -> Int -> Coordinate
makeCoord row hole = 
    if hole < 1 then error ("Illegal hole number: " ++ show hole ++ " < 1")
    else if hole > row then error ("Illegal hole number: " ++ show hole ++ " on row " ++ show row)
    else Coordinate row hole

instance Show Coordinate where
    show (Coordinate r h) = "r" ++ show r ++ "h" ++ show h

possibleMoves :: Coordinate -> Int -> [Move] 
possibleMoves c@(Coordinate r h) rowCount = upLeftMove ++ upRightMove ++ leftMove ++ rightMove ++ downLeftMove ++ downRightMove
    where 
        upLeftMove = if r >= 3 && h >= 3 then [Move c (makeCoord (r - 1) (h - 1)) (makeCoord (r - 2) (h - 2))] else []
        upRightMove = if r >= 3 && r - h >= 2 then [Move c (makeCoord (r - 1) h) (makeCoord (r - 2) h)] else []         
        leftMove = if h >= 3 then [Move c (makeCoord r (h - 1)) (makeCoord r (h - 2))] else []
        rightMove = if r - h >= 2 then [Move c (makeCoord r (h + 1)) (makeCoord r (h + 2))] else []
        downLeftMove = if rowCount - r >= 2 then [Move c (makeCoord (r + 1) h) (makeCoord (r + 2) h)] else []
        downRightMove = if rowCount - r >= 2 then [Move c (makeCoord (r + 1) (h + 1)) (makeCoord (r + 2) (h + 2))] else []




data Move = Move {from :: Coordinate, jumped :: Coordinate, to :: Coordinate}

instance Show Move where
    show (Move f j t) = show f ++ " -> " ++ show j ++ " -> " ++ show t




data GameState =  GameState {rowCount :: Int, occupiedHoles :: [Coordinate]}

makeState :: Int -> Coordinate -> GameState
makeState rows emptyHole = GameState rows pegs
    where pegs = [makeCoord row hole | row <- [1..rows], hole <- [1..row], makeCoord row hole /= emptyHole]

performMove :: GameState -> Move -> GameState
performMove (GameState rows pegs) (Move f j t) = 
    if not (f `elem` pegs) then error "Move is not consistent with game state: 'from' hole was unoccupied."
    else if not (j `elem` pegs) then error "Move is not consistent with game state: jumped hole was unoccupied."
    else if t `elem` pegs then error "Move is not consistent with game state: 'to' hole was occupied."
    else if row t > rows || row t < 1 then error ("Move is not legal because the 'to' hole does not exist: " ++ show t)
    else GameState rows (t : delete f (delete j pegs))

legalMoves :: GameState -> [Move]
legalMoves (GameState rows pegs) = [m | c <- pegs, m <- possibleMoves c rows, jumped m `elem` pegs, not (to m `elem` pegs)]

instance Show GameState where
    show (GameState rows pegs) = header ++ concatMap printRow [1..rows]
        where 
            header = "Game with " ++ show (length pegs) ++ " pegs:\n"
            printCoord row hole = if makeCoord row hole `elem` pegs then " *" else " O"
            printRow row = replicate (rows - row) ' ' ++ concatMap (printCoord row) [1..row] ++ "\n"

           




search :: GameState -> [Move] -> ([[Move]], Int)
search gs moves = search' gs moves [] 0

search' :: GameState -> [Move] -> [[Move]] -> Int -> ([[Move]], Int)
search' gs moves solutions gamesPlayed = 
    if length (occupiedHoles gs) == 1 then (moves : solutions, gamesPlayed + 1)
    else if null nextMoves then (solutions, gamesPlayed + 1)
    else foldl' combineSearches (solutions, gamesPlayed) nextMoves
        where 
            nextMoves = legalMoves gs
            combineSearches (s, gp) move = search' (performMove gs move) (move : moves) s gp

    
                



main :: IO ()
main = do
        startTime <- ((round `fmap` getPOSIXTime)::IO Int)
        let gs = makeState 5 (makeCoord 3 2)
        let (solutions, gamesPlayed) = search gs []
        endTime <- round `fmap` getPOSIXTime        
        putStrLn ("Games played:    " ++ show gamesPlayed)
        putStrLn ("Solutions found: " ++ show (length solutions))
        putStrLn ("Time elapsed:    " ++ show ((endTime - startTime) `div` 1000000))



