module Main where

import Debug.Trace (trace)

import Control.Monad (liftM)
import Data.Maybe (isNothing)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Random (randomR, random, getStdRandom)

import Text.JSON
import HG.Proto

playerCount = 2
boardSize = 8

data Config = Config { listenPort :: Integer
                     , gameID :: Integer
                     }

data Player = Player { playerSock :: Handle
                     , playerID :: PlayerID
                     }
              deriving (Eq, Show)

data Game = Game { gameBoard :: Board
                 , gamePlayers :: [Player]
                 , currentPlayer :: Player
                 , gameTurns :: Integer
                 , gameStatus :: GameStatus}
          deriving (Eq, Show)

data GameStatus = Playing
                | Finished { winner :: Maybe Player
                           , reason :: FinishReason }
                deriving (Eq, Show)

data FinishReason = Stalemate
                  | Disconnect
                  | PlayerWin
                  | BadMessage
                  | BadMove
                  deriving (Eq, Show)

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

type Board = [[Maybe PlayerID]]

addToBoard :: Coord -> PlayerID -> Board -> Board
addToBoard (x, y) pid b = brow ++ [bcol ++ [Just pid] ++ acol] ++ arow
  where (brow, arow') = splitAt (fromInteger x) b
        row = head arow'
        arow = tail arow'
        (bcol, acol') = splitAt (fromInteger y) row
        acol = tail acol'

getFromBoard (x, y) b = (b !! fromInteger x) !! fromInteger y

-- Can the given player place a stone here
validMove :: Coord -> Player -> Board -> Bool
validMove c p b = notTaken && aPieceChanged
  where notTaken = isNothing $ getFromBoard c b
        aPieceChanged = all (\d -> changeInDirection c p d b) directions

-- Can any player place a stone here
openPlace :: Coord -> Game -> Bool
openPlace c g = any (\p -> validMove c p (gameBoard g)) ps
  where ps = gamePlayers g
        b = gameBoard g

mapBoardC :: (Coord -> Maybe PlayerID -> a) -> Board -> [[a]]
mapBoardC f b = zipWith mapB' b [0..]
  where mapB' r x = zipWith (\v y -> f (x, y) v) r [0..]

mapBoard f b = map mapB' b
  where mapB' r = map f r

changeInDirection :: Coord -> Player -> Direction -> Board -> Bool
changeInDirection c p d b = changeInDirection' (changer c)
  where changeInDirection' c@(x, y)
        -- edges
          | x <= 0 || y <= 0 || x > 8 || y > 8 = False
        -- my stone
          | stone == Just (playerID p) = True
        -- no stone
          | isNothing stone = False
        -- enemy stone
          | otherwise = changeInDirection' (changer c)
          where stone = getFromBoard c b
        changer = snd (head (filter (\p -> fst p == d) directionModifiers))

swapStones :: Coord -> Player -> Direction -> Board -> Board
swapStones c p d b = swapStones' (changer c) b
  where swapStones' c b
          | stone == Just (playerID p) = b
          | isNothing stone = b
          | otherwise = swapStones' (changer c) (addToBoard c (playerID p) b)
          where stone = getFromBoard c b
        changer = snd (head (filter (\p -> fst p == d) directionModifiers))

liftBoard :: (Board -> Board) -> Game -> Game
liftBoard f g = g {gameBoard = f (gameBoard g)}

gameEnded :: Game -> Bool
gameEnded g = all not (concat validOpenPlaceMap)
  where validOpenPlaceMap = mapBoardC (\ c v -> isNothing v && openPlace c g)
                            (gameBoard g)

playerCanMove :: Player -> Board -> Bool
playerCanMove p b = or (concat validPlaceMap)
  where validPlaceMap = mapBoardC (\c v-> isNothing v && validMove c p b) b

type Coord = (Integer, Integer)

type PlayerID = Integer

doMove :: Coord -> Game -> Game
doMove c g
  | gameEnded g' = declareWinner g'
  | playerCanMove np b' = g' { currentPlayer = np
                             , gameTurns = gameTurns g + 1}
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

-- Reads a move in the forma
readMove :: (JSObject (Integer, Integer)) -> Maybe Coord
readMove jsobj
  | length vals /= 1 ||
    (fst . head $ vals) /= "placed" = Nothing
  | otherwise = Just . snd . head $ vals
  where vals = fromJSObject jsobj

receiveMove :: Player -> IO (Maybe Coord)
receiveMove p = liftM (>>= readMove)
                (receiveMessage $ playerSock p)

updatePlayers :: Game -> Game -> IO Game
updatePlayers g g' = do
  let b = gameBoard g
      b' = gameBoard g'
      diff = fst $ diffBoards (currentPlayer g) b b'
      message = generateUpdateMessage diff g g'
  mapM_ (\p -> sendMessage (playerSock p) message)
    (gamePlayers g)
  return g'

generateUpdateMessage :: [Coord] -> Game -> Game -> JSObject JSValue
generateUpdateMessage diff g g' = toJSObject
  [ ("changed", showJSON diff)
  , ("by", showJSON (playerID (currentPlayer g)))
  , ("next_turn", showJSON (playerID (currentPlayer g')))
  , ("game_status", showJSON (statusToInteger (gameStatus g')))
  ]

diffBoards :: Player -> Board -> Board -> ([Coord], PlayerID)
diffBoards p b b' = (changed, playerID p) -- At least one stone
                  -- must always changed
  where changed = foldl (\cs (c, v) -> if v /= -1
                                       then c:cs
                                       else cs) [] coords
        coords = concat $ mapBoardC worker b'
        worker c (Just v) = if getFromBoard c b /= Just v
                            then (c, v)
                            else (c, -1)
        worker c Nothing = (c, -1)

-- A blank board
newBoard :: Board
newBoard = replicate 8 (replicate 8 Nothing)

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

newGame :: [Player] -> Player -> Game
newGame ps c = Game (setupBoard ps newBoard) ps c 0 Playing

serializeGame :: Game -> Player -> JSObject JSValue
serializeGame g p = toJSObject [ ("board", board)
                               , ("next_turn", current)
                               , ("you", you)]
  where board = showJSON $ mapBoard worker (gameBoard g)
          where worker (Just v) = v
                worker Nothing = -1
        current = showJSON . playerID . currentPlayer $ g
        you = showJSON . playerID $ p

chooseStarter :: [Player] -> IO Player
chooseStarter players = fmap (players !!) chosen
  where chosen = getStdRandom (randomR (0, length players - 1))

notCurrentPlayer :: Game -> Player
notCurrentPlayer g = head $
                     dropWhile (== currentPlayer g) (cycle (gamePlayers g))

cyclePlayer :: Game -> Game
cyclePlayer g = g { currentPlayer = notCurrentPlayer g }

changeStatus :: Game -> GameStatus -> Game
changeStatus g status = g {gameStatus = status}

fullSync :: Game -> [Player] -> IO Game
fullSync g ps = mapM_ (\p -> sendMessage (playerSock p) (serializeGame g p)) ps
                >> return g

startGame :: [Player] -> IO Game
startGame players = do
  starter <- chooseStarter players
  let game = newGame players starter
  game' <- fullSync game players
  putStrLn "Completed full gamestate sync"
  gameLoop game'

finishGame :: Game -> Maybe Player -> FinishReason -> Game
finishGame g p r = g {gameStatus = Finished p r}

gameLoop :: Game -> IO Game
gameLoop g
  | gameStatus g /= Playing = return g
  | otherwise = do
    mmove <- receiveMove (currentPlayer g)
    putStrLn $ "Recived move from " ++ (show . playerID . currentPlayer $ g)
    case mmove of
      Nothing -> return $ finishGame g (Just $ notCurrentPlayer g) BadMessage
      Just coord -> updatePlayers g g' >>= gameLoop
        where g' = executeMove coord g

executeMove :: Coord -> Game -> Game
executeMove c g
  | validMove c (currentPlayer g) (gameBoard g) = doMove c g
  | otherwise = finishGame g (Just $ notCurrentPlayer g) BadMove

main :: IO()
main = withSocketsDo $ do
  config <- getArgs >>= parseArgs
  cons <- reciveConnections playerCount (listenPort config)
  let players = map (\x -> Player (snd x) (fst x)) cons
  putStrLn $ "Recived connections, " ++ show (length $ players) ++
    " players: " ++ (show $ map playerID players)
  startGame players
  cleanupConnections $ map playerSock players
  return ()

parseArgs :: [String] -> IO Config
parseArgs (port:gameID:[]) = return $ Config (read port) (read gameID)
parseArgs _ = putStrLn "Bad arguments" >> exitFailure