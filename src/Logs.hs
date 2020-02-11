{-# LANGUAGE OverloadedStrings #-}
module Logs(
  writeGame,
  clearLog,
  clearAllLogs,
  )
where

import Data.Time.Clock.System (systemSeconds, getSystemTime)
import Data.Csv
import Data.List (sort, sortBy, elemIndex)
import System.FilePath
import System.Directory (createDirectoryIfMissing,
                         removeDirectoryRecursive,
                         doesDirectoryExist,
                         doesFileExist)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Cards
import Hearts.Types

instance ToField HandScore where
  toField = toField . score

instance ToField Suit where
  toField = toField . show

log_dir :: FilePath
log_dir = "logs/"

playerDir :: PlayerId -> FilePath
playerDir playerId = log_dir </> takeBaseName playerId

logName :: String -> String -> FilePath
logName stamp player_id = playerDir player_id </> stamp <.> "csv"

getUniqueStamp :: [PlayerId] -> IO String
getUniqueStamp players = do
  time <- getSystemTime
  let stamp = show $ systemSeconds time
      logs = map (logName stamp) players
  exists <- mapM doesFileExist logs
  if or exists
    then threadDelay 1000 >> getUniqueStamp players
    else return stamp

foldStr :: (a -> String) -> String -> [a] -> String
foldStr _ _ [] = ""
foldStr f sep (x: xs) = foldl (\ys y -> ys ++ sep ++ f y) (f x) xs

fromCards :: [Card] -> String
fromCards cards = foldStr toId "," cards
  where
    toId (Card suit rank) = show suit ++ r
      where
        r | rank < Jack = show (fromEnum rank + 2)
          | otherwise = take 1 (show rank)

toCards :: [Trick] -> String
toCards tricks = foldStr getCards ";" tricks
  where
    getCards = fromCards . map first

clearLog :: Player -> IO ()
clearLog Player{playerId=p} = do
  exist <- doesDirectoryExist player_dir
  if exist
    then removeDirectoryRecursive player_dir
    else return ()
  where
    player_dir = (playerDir p)

clearAllLogs :: [Player] -> IO ()
clearAllLogs players = mapM_ clearLog players

writeHand :: [Player] -> String -> HandResult -> IO ()
writeHand players stamp (HandResult tricks scores) = mapM_
  (\(p, q) -> BL.appendFile (logName stamp p) (encode [row p q]))
  (zip pids points)
  where
    -- Extract player ids and sort them
    pids = (sort . getPids) players
    points = sortBy (\HandScore{playerId=p} HandScore{playerId=q} -> compare p q) scores
    ordered = (reverse . map sortTrick) tricks
    start = elemIndex (((\Player{playerId} -> playerId) . head) players) pids
    row player result = ( stamp
                        , elemIndex player pids
                        , result
                        , start
                        , toCards ordered)

-- Careful, needs to be sorted on PlayerId
sortTrick :: [(a, b, PlayerId)] -> [(a, b, PlayerId)]
sortTrick = sortBy (\x y -> compare (third x) (third y))

getPids :: [Player] -> [PlayerId]
getPids =  map (\Player{playerId} -> playerId)

writeGame :: GameResult -> IO ()
writeGame (GameResult plays _ players) = do
  stamp <- getUniqueStamp pids
  mapM_ (createDirectoryIfMissing True) (map playerDir pids)
  mapM_ (writeHand players stamp) (reverse plays)
  where
    pids = getPids players
