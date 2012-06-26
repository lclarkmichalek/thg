module HG.Engines.TwoPlayerBoard
       ( PlayerID(..)
       , Coord(..)
       , Player(..)
       , Move(..)
       , GameImpl(..)
       , Config(..)
       , Board(..)
       , BoardToken(..)
       , Game(..)
       , GameStatus(..)
       , FinishReason(..)

       , createSquareBoard
       , createRectangularBoard

       , runGame

       , liftBoard
       , notCurrentPlayer
       , addToBoard
       , getFromBoard
       , finishGame
       ) where

import HG.Proto

import System.IO (hGetLine, hPutStrLn, hClose, hFlush, Handle)
import Text.JSON

import Control.Monad(join)

type PlayerID = Integer

type Coord = (Integer, Integer)

data Player = Player { playerID :: PlayerID
                     , playerSock :: Handle
                     } deriving (Eq, Show)

data Move = Move { moveFrom :: Maybe Coord
                 , moveTo :: Maybe Coord
                 , movePlayer :: Player
                 } deriving (Eq, Show)

data GameImpl = GameImpl { gameWon :: (Game -> Maybe Player)
                         , applyMove :: (Game -> Move -> Game)
                         , playerCount :: Integer
                         , createGame :: ([Player] -> Player -> Game)
                         , chooseStarter :: ([Player] -> IO Player)
                         }

data Config = Config { listenPort :: Integer }

type Board = [[BoardToken]]

type BoardToken = Maybe PlayerID

data Game = Game { gameBoard :: Board
                 , gamePlayers :: [Player]
                 , currentPlayer :: Player
                 , gameStatus :: GameStatus
                 } deriving (Eq, Show)

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

-- Creates an n by n board filled with Nothing
createSquareBoard :: Int -> Board
createSquareBoard n = replicate n (replicate n Nothing)

-- Creates a rectangular board filled with Nothing
createRectangularBoard :: Int -> Int -> Board
createRectangularBoard w h = replicate w (replicate h Nothing)

statusToInteger :: GameStatus -> Integer
statusToInteger Playing = 0
statusToInteger (Finished _ PlayerWin) = 1
statusToInteger (Finished _ Stalemate) = 2
statusToInteger (Finished _ Disconnect) = 3
statusToInteger (Finished _ BadMessage) = 4
statusToInteger (Finished _ BadMove) = 5

addToBoard :: Coord -> PlayerID -> Board -> Board
addToBoard (x, y) pid b = brow ++ [bcol ++ [Just pid] ++ acol] ++ arow
  where (brow, (row:arow)) = splitAt (fromIntegral x - 1) b
        (bcol, (col:acol)) = splitAt (fromIntegral y - 1) row

getFromBoard (x, y) b = (b !! fromInteger (x - 1)) !! fromInteger (y - 1)

mapBoardCoords :: (Coord -> BoardToken -> a) -> Board -> [[a]]
mapBoardCoords f b = zipWith mapB' b [1..]
  where mapB' r x = zipWith (\v y -> f (x, y) v) r [1..]

mapBoard :: (BoardToken -> a) -> Board -> [[a]]
mapBoard f b = map (map f) b

--
liftBoard :: (Board -> Board) -> Game -> Game
liftBoard f g = g {gameBoard = f (gameBoard g)}

-- Runs the game with the given implementation and config
runGame :: GameImpl -> Config -> IO()
runGame impl config = withSocketsDo $ do
  cons <- reciveConnections (playerCount impl) (listenPort config)
  putStrLn "Received connections"
  let players = map (\x -> Player (fst x) (snd x)) cons

  starter <- (chooseStarter impl) players
  putStrLn "Chosen starter"
  let game = (createGame impl) players starter
  game' <- fullSync game players
  putStrLn "Initial fullSync run"
  gameLoop game' impl

  cleanupConnections $ map playerSock players
  return ()

serializeGame :: Game -> Player -> JSObject JSValue
serializeGame g p = toJSObject [ ("board", board)
                               , ("next_turn", current)
                               , ("you", you)]
  where board = showJSON $ mapBoard worker (gameBoard g)
          where worker (Just v) = v
                worker Nothing = -1
        current = showJSON . playerID . currentPlayer $ g
        you = showJSON . playerID $ p

gameLoop :: Game -> GameImpl -> IO Game
gameLoop g impl
  | gameStatus g /= Playing = return g
  | otherwise = do
    mmove <- receiveMove (currentPlayer g)
    putStrLn $ "Recived move from " ++ (show . playerID . currentPlayer $ g)
    case mmove of
      Nothing -> return $ finishGame g (Just $ notCurrentPlayer g) BadMessage
      Just move -> updatePlayers g g' >>= flip gameLoop impl
        where g' = applyMove impl g move

fullSync :: Game -> [Player] ->  IO Game
fullSync g ps = mapM_ (\p -> sendMessage (playerSock p) (serializeGame g p)) ps
                >> return g

getMap :: Eq a => [(a, b)] -> a -> Maybe b
getMap ((a, b):xs) t
  | a == t = Just b
  | length xs == 0 = Nothing
  | otherwise = getMap xs t

readMove :: Player -> (JSObject (Integer, Integer)) -> Maybe Move
readMove player jsobj
  | to == Nothing = Nothing
  | otherwise = Just $ Move from to player
  where vals = fromJSObject jsobj
        from = getMap vals "from"
        to = getMap vals "to"

-- Receives a move from a player. Nothing if the move couldn't be parsed
receiveMove :: Player -> IO (Maybe Move)
receiveMove p = do
  msg <- receiveMessage $ playerSock p
  return $ join $ fmap (readMove p) msg

finishGame :: Game -> Maybe Player -> FinishReason -> Game
finishGame g p r = g {gameStatus = Finished p r}

notCurrentPlayer :: Game -> Player
notCurrentPlayer g = head $
                     dropWhile (== currentPlayer g) (cycle (gamePlayers g))

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
        coords = concat $ mapBoardCoords worker b'
        worker c (Just v) = if getFromBoard c b /= Just v
                            then (c, v)
                            else (c, -1)
        worker c Nothing = (c, -1)
