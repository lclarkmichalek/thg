module Main where

import HG.Engines.TwoPlayerBoard

import Data.List (transpose)
import Data.Maybe (catMaybes, isNothing)

import Control.Monad (msum)

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Random (randomR, random, getStdRandom)


impl :: GameImpl
impl = GameImpl { gameWon = gameWon'
                , applyMove = applyMove'
                , playerCount = 2
                , createGame = createGame'
                , chooseStarter = chooseStarter'
                }

newBoard :: Board
newBoard = replicate 3 (replicate 3 Nothing)

applyMove' :: Game -> Move -> Game
applyMove' g m = case moveTo m of
  Nothing -> finishGame g (Just $ notCurrentPlayer g) BadMessage
  Just move -> if validMove move (currentPlayer g) (gameBoard g)
               then liftBoard (doMove move (currentPlayer g)) g
               else finishGame g (Just $ notCurrentPlayer g) BadMove

validMove :: Coord -> Player -> Board -> Bool
validMove (x, y) p b
  | x > 2 || x < 0 || y > 2 || y < 0 = False
  | otherwise = isNothing (b !! (fromIntegral x) !! (fromIntegral y))

doMove :: Coord -> Player -> Board -> Board
doMove (x, y) p b = addToBoard (x, y) (playerID p) b

createGame' :: [Player] -> Player -> Game
createGame' ps p = Game newBoard ps p Playing

chooseStarter' :: [Player] -> IO Player
chooseStarter' players = fmap (players !!) chosen
  where chosen = getStdRandom (randomR (0, length players - 1))

findPlayer :: [Player] -> PlayerID -> Maybe Player
findPlayer (p:ps) pid
  | (playerID p) == pid = Just p
  | otherwise = findPlayer ps pid
findPlayer _ _ = Nothing

gameWon' :: Game -> Maybe Player
gameWon' game = (findPlayer (gamePlayers game)) =<< pid
  where pid :: Maybe PlayerID
        pid = do
          checkDiagonal b
          msum $ map checkRow b
          msum $ map checkRow (transpose b)
        b = gameBoard game
        checkRow :: [Maybe PlayerID] -> Maybe PlayerID
        checkRow row
          | length (catMaybes row) /= 3 = Nothing
          | otherwise = foldl (\p c -> if isNothing p || p /= c
                                       then Nothing
                                       else p) (head row) (tail row)
        checkDiagonal b = checkRow [b !! 0 !! 0, b !! 1 !! 1, b !! 2 !! 2] >>
                          checkRow [b !! 2 !! 0, b !! 1 !! 1, b !! 0 !! 2]

main :: IO()
main = do
  config <- getArgs >>= parseArgs
  runGame impl config

parseArgs :: [String] -> IO Config
parseArgs (port:[]) = return $ Config (read port)
parseArgs _ = putStrLn "Bad arguments" >> exitFailure