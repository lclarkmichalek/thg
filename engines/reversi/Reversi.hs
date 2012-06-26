module Main where

import HG.Engines.TwoPlayerBoard

import Debug.Trace (trace)

import Control.Monad (liftM)
import Data.Maybe (isNothing, catMaybes)
import Data.List(maximumBy, group, sort)
import Data.Ord(comparing)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Random (randomR, random, getStdRandom)

import Text.JSON
import HG.Proto

boardSize = 8

impl :: GameImpl
impl = GameImpl { gameWon = gameWon'
                , applyMove = applyMove'
                , playerCount = 2
                , createGame = createGame'
                , chooseStarter = chooseStarter'
                }

createGame' :: [Player] -> Player -> Game
createGame' ps p = Game { gameBoard = setupBoard ps (createSquareBoard 8)
                        , gamePlayers  = ps
                        , currentPlayer = p
                        , gameStatus = Playing
                        }

chooseStarter' :: [Player] -> IO Player
chooseStarter' players = fmap (players !!) chosen
  where chosen = getStdRandom (randomR (0, length players - 1))

applyMove' :: Game -> Move -> Game
applyMove' g m = case moveTo m of
  Nothing -> finishGame g (Just $ notCurrentPlayer g) BadMessage
  Just move -> if validMove move (currentPlayer g) (gameBoard g)
               then doMove move g
               else finishGame g (Just $ notCurrentPlayer g) BadMove

executeMove :: Coord -> Game -> Game
executeMove c g
  | validMove c (currentPlayer g) (gameBoard g) = doMove c g
  | otherwise = finishGame g (Just $ notCurrentPlayer g) BadMove

gameWon' :: Game -> Maybe Player
gameWon' game
  | fullBoard game = mostPlaced game
  | otherwise = Nothing

fullBoard game = (filter (/= []) cols) == []
  where cols = map (filter (==Nothing)) (gameBoard game)

mostPlaced game = findPlayer (gamePlayers game) winner
  where vals = catMaybes $ concat (gameBoard game)
        winner = mostCommon vals

findPlayer :: [Player] -> PlayerID -> Maybe Player
findPlayer (p:ps) pid
  | (playerID p) == pid = Just p
  | otherwise = findPlayer ps pid
findPlayer _ _ = Nothing

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (comparing length) . group . sort

data Direction = N | NE | E | SE | S | SW | W | NW
               deriving (Eq, Show)

directions = [N, NE, E, SE, S, SW, W, NW]
directionModifiers = [ (N, \(x, y) -> (x, y+1))
                     , (NE, \(x, y) -> (x+1, y+1))
                     , (E, \(x,y) -> (x+1, y))
                     , (SE, \(x, y) -> (x+1, y-1))
                     , (S, \(x, y) -> (x, y-1))
                     , (SW, \(x, y) -> (x-1, y-1))
                     , (W, \(x, y) -> (x-1, y))
                     , (NW, \(x, y) -> (x-1, y+1))
                     ]

onVertical :: Coord -> Coord -> Bool
onVertical (x1, _) (x2, _) = x1 == x2

onHorizontal :: Coord -> Coord -> Bool
onHorizontal (_, y1) (_, y2) = y1 == y2

onDiagonal :: Coord -> Coord -> Bool
onDiagonal (x1, y1) (x2, y2) = abs ((fromIntegral $ x1 - x2)/
                                    (fromIntegral $ y1 - y2)) == 1

onNWDiagonal :: Coord -> Coord -> Bool
onNWDiagonal (x1, y1) (x2, y2) = (fromIntegral $ x1 - x2)/
                                 (fromIntegral $ y1 - y2) == -1

onNEDiagonal :: Coord -> Coord -> Bool
onNEDiagonal (x1, y1) (x2, y2) = (fromIntegral $ x1 - x2)/
                                 (fromIntegral $ y1 - y2) == 1

-- Converts the GameStatus to an integer as defined by the reversi
-- protocol in protocol.md
statusToInteger :: GameStatus -> Integer
statusToInteger Playing = 0
statusToInteger (Finished _ PlayerWin) = 1
statusToInteger (Finished _ Stalemate) = 2
statusToInteger (Finished _ Disconnect) = 3
statusToInteger (Finished _ BadMessage) = 4
statusToInteger (Finished _ BadMove) = 5

-- Can the given player place a stone here
validMove :: Coord -> Player -> Board -> Bool
validMove c p b = notTaken && aPieceChanged
  where notTaken = isNothing $ getFromBoard c b
        aPieceChanged = any (\d -> changeInDirection c p d b) directions

-- Can any player place a stone here
openPlace :: Coord -> Game -> Bool
openPlace c g = any (\p -> validMove c p (gameBoard g)) ps
  where ps = gamePlayers g
        b = gameBoard g

mapBoardC :: (Coord -> Maybe PlayerID -> a) -> Board -> [[a]]
mapBoardC f b = zipWith mapB' b [1..]
  where mapB' r x = zipWith (\v y -> f (x, y) v) r [1..]

mapBoard f b = map (map f) b

changeInDirection :: Coord -> Player -> Direction -> Board -> Bool
changeInDirection c p d b = changeInDirection' (changer c) False
  where changeInDirection' c@(x, y) t
        -- edges
          | x <= 0 || y <= 0 || x > 8 || y > 8 = False
        -- my stone
          | stone == Just (playerID p) && t = True
        -- no stone
          | isNothing stone = False
        -- enemy stone
          | otherwise = changeInDirection' (changer c) True
          where stone = getFromBoard c b
        changer = snd (head (filter (\p -> fst p == d) directionModifiers))

swapStones :: Coord -> Player -> Direction -> Board -> Board
swapStones c p d b = swapStones' (changer c) (addToBoard c (playerID p) b)
  where swapStones' c b
          | stone == Just (playerID p) = b
          | isNothing stone || (not $ changeInDirection c p d b) = b
          | otherwise = swapStones' (changer c) (addToBoard c (playerID p) b)
          where stone = getFromBoard c b
        changer = snd (head (filter (\p -> fst p == d) directionModifiers))

gameEnded :: Game -> Bool
gameEnded g = all not (concat validOpenPlaceMap)
  where validOpenPlaceMap = mapBoardC (\ c v -> isNothing v && openPlace c g)
                            (gameBoard g)

playerCanMove :: Player -> Board -> Bool
playerCanMove p b = or (concat validPlaceMap)
  where validPlaceMap = mapBoardC (\c v-> isNothing v && validMove c p b) b

doMove :: Coord -> Game -> Game
doMove c g
  | gameEnded g' = declareWinner g'
  | playerCanMove np b' = g' { currentPlayer = np}
  where g' = liftBoard (updateBoard c p) g
        np = notCurrentPlayer g
        p = currentPlayer g
        b = gameBoard g
        b' = gameBoard g'

-- Declares the current player the winner
declareWinner :: Game -> Game
declareWinner g = g {gameStatus = gs}
  where gs = Finished (Just $ currentPlayer g) PlayerWin

updateBoard :: Coord -> Player -> Board -> Board
updateBoard c p b = foldl (flip (swapStones c p)) b directions

-- Reads a move in the form
readMove :: (JSObject (Integer, Integer)) -> Maybe Coord
readMove jsobj
  | length vals /= 1 ||
    (fst . head $ vals) /= "placed" = Nothing
  | otherwise = Just . snd . head $ vals
  where vals = fromJSObject jsobj

placeStone :: Coord -> PlayerID -> Board -> Board
placeStone = addToBoard

placeStones :: Board -> [Coord] -> PlayerID -> Board
placeStones b coords pid = foldl (\b c -> placeStone c pid b) b coords

-- Sets up the reversi board with the standard bwbw pattern in the
        -- middle square
setupBoard :: [Player] -> Board -> Board
setupBoard players b = placedSecond
  where pids = map playerID players
        firstPid = pids !! 0
        secondPid = pids !! 1
        placedFirst = placeStones b [(4, 4), (5, 5)] firstPid
        placedSecond = placeStones placedFirst [(4, 5), (5, 4)] secondPid

cyclePlayer :: Game -> Game
cyclePlayer g = g { currentPlayer = notCurrentPlayer g }

changeStatus :: Game -> GameStatus -> Game
changeStatus g status = g {gameStatus = status}

main :: IO()
main = withSocketsDo $ do
  config <- getArgs >>= parseArgs
  runGame impl config

parseArgs :: [String] -> IO Config
parseArgs (port:[]) = return $ Config (read port)
parseArgs _ = putStrLn "Bad arguments" >> exitFailure