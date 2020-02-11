{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Trustworthy #-}
-- | Module for /tournament/ handling -- e.g., compute elo changes and the like.
module Game where

import System.Random
import System.Timeout
import Control.Monad
import Data.Foldable
import Data.List
import Data.Monoid
import Data.Maybe(fromMaybe)
import EitherIO

import Hearts.Types

data Place = Place {
  playerId :: PlayerId,
  place :: Int,
  eloPre :: Float
} deriving (Show)

initElo :: Float
initElo = 1200

sameField :: Eq a => (t -> a) -> t -> t -> Bool
sameField f a b = f a == f b

newPlayer :: PlayerId -> PlayFunc -> BidFunc -> Player
newPlayer playerId play _ = oldPlayer playerId play () initElo 0

oldPlayer :: PlayerId -> PlayFunc -> BidFunc -> Float -> Int -> Player
oldPlayer playerId play _ elo gamesPlayed =
  Player playerId play elo gamesPlayed

placesFromGameScores
  :: [Player]
  -> [GameScore]
  -> [Place]
placesFromGameScores players = concatMap (\(p, s)->map (makePlace p) s)
  . zip [1..]
  . groupBy (sameField finalScore)
  . reverse
  . sortOn finalScore
  where
  getElo i = fromMaybe initElo $
    elo <$> find (\Player{playerId=pid}->pid == i) players
  makePlace place (GameScore (Player playerId _ _ _) _) =
    Place playerId place (getElo playerId)

-- https://github.com/qwhex/multi_elo/blob/master/multi_elo.py
computeEloChanges :: [Place] -> [Float]
computeEloChanges places = map calcElo places
  where n = fromIntegral $ length places
        k = 32.0 / (n-1.0)
        calcElo Place{playerId=pid, place=pPlace, eloPre=pElo} =
          let opponents = filter (\Place{playerId=oid} -> oid /= pid) places
              calcS oPlace
                | pPlace < oPlace = 1.0
                | pPlace == oPlace = 0.5
                | otherwise = 0
              calcEA oElo = 1.0 / (1.0 + 10.0 ** ((oElo - pElo)/400.0))
              opponentEloContrib o = let s = calcS (place o)
                                         ea = calcEA (eloPre o)
                                     in k * (s-ea)
          in sum $ map opponentEloContrib opponents

-- | Update a player with a new game
updatePlayer :: Player -> Float -> Player
updatePlayer p@Player{gamesPlayed=oldGames, elo=oldElo} eloChange =
  p{gamesPlayed=succ oldGames, elo=oldElo + eloChange}
