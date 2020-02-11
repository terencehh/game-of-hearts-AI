{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hearts.Types where

import Cards
import Data.List (elemIndex)

type PlayerId = String
type Play = (Card, String, PlayerId)
type Trick = [Play]

data GameError = GameError PlayerError PlayerId String
  deriving Show

data PlayerError = RenegError       -- ^ Did not follow suit
                 | InvalidCardError -- ^ Card not in hand
                 | BidError         -- ^ Not in use in Hearts
                 | TimeError        -- ^ Choice too slow
                 | LeadError        -- ^ Did not open with C2
                 | BleedError       -- ^ Played a H first round
                 | BrokenError
                 -- ^ Lead with hearts before any were played
  deriving Show

data Player = Player {
  playerId :: PlayerId,
  playFunc :: PlayFunc,
  elo :: Float,
  gamesPlayed :: Int
}

instance Eq Player where
  Player{playerId=a} == Player{playerId=b} = a == b

instance Show Player where
  show Player{playerId, elo, gamesPlayed} = "Player: " ++ show playerId
                                        ++", games played: " ++ show gamesPlayed
                                        ++", elo: " ++ show elo

data Hand = Hand {
  player :: Player,
  cards :: [Card]
}

data GameResult = GameResult {
  hands :: [HandResult],
  gameScore :: [GameScore],
  updatedPlayers :: [Player]
}

data HandResult = HandResult {
  tricks :: [Trick],
  scores :: [HandScore]
}

data HandScore = HandScore {
  playerId :: PlayerId,
  score :: Int
} deriving Show

data GameScore = GameScore {
  player :: Player,
  finalScore :: Int
} deriving (Eq)

instance Show GameScore where
  show (GameScore Player{playerId} score) = "Player: " ++ show playerId ++
    ", final score: " ++ show score

-- | Play function type.
--
-- Cards are added to each trick from the front.  Thus, the first-played card
-- in a trick is the last in its list of cards The lead suit is the suit of the
-- first-played card in the trick.  A play is only legal if it does not reneg
-- on the lead suit.  That is, the player must play a card of the suit led if
-- they have it.  The winner is the highest trump card (if any were legally
-- played), or, if no trumps were played, the highest card in the lead suit.
type PlayFunc
  =  PlayerId -- ^ this player's Id so they can identify themselves in the bids
              -- and tricks
  -> [Card]   -- ^ the player's cards
  -> [(Card, PlayerId)]                 -- ^ cards in the current trick, so far
  -> Maybe ([(Card, PlayerId)], String) -- ^ previous player's state
  -> (Card, String)             -- ^ the player's chosen card and new state

type BidFunc = ()

-- Convenience conversion functions.

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

no_snd :: (a, b, c) -> (a, c)
no_snd (x, _, z) = (x, z)

-- Logs utility

log_header :: [(String, String, String, String, String)]
log_header = [("time", "pos", "score", "first", "tricks")]
