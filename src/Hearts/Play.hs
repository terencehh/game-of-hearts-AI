-- | Module to play a round of Hearts.  Distribute a deck of cards between two
-- or four players and play games until one player reaches a score of a
-- hundred.  Winner is the one with the lowest score.
module Hearts.Play where

-- System
import qualified Control.Monad.HT as M
import qualified Data.Map as Map
import System.Timeout
import Data.List
import Control.DeepSeq

-- Tournament
import Game
import Cards
import Deck
import EitherIO

-- Game used
import Hearts.Rules
import Hearts.Types

-- Play game

-- | Play out a full game, then update the players.
playGame
  :: Int                        -- ^ max score for the game
  -> [Player]                   -- ^ a list of players
  -> EitherIO GameError GameResult
  -- ^ played-out hand results, scores and places for each player and
  -- players with updated elos
playGame max_score players = do
  (GameResult played results _) <- playUntil max_score nilResult
  let places = placesFromGameScores players results
      elosById = map snd $ sort
                 $ zip (map (playerId :: Place -> PlayerId) places)
                 $ computeEloChanges places
      playersById = sortOn (playerId :: Player -> PlayerId) players
      elo_players = zipWith updatePlayer playersById elosById
      updatedPlayers = reverse . sortOn elo $ elo_players
  return $ GameResult played results updatedPlayers

  where
    nilScore = map ((flip GameScore) 0) players
    nilResult = GameResult [] nilScore players

-- | Play hands until we reach the @max_score@.
playUntil :: Int -> GameResult -> EitherIO GameError GameResult
playUntil max_score results
  | hasScore max_score results = return results
  | otherwise = playDeck results >>= playUntil max_score

-- | The game stops when:
--
--  - one player has over 'max_score' points; and,
--  - exactly one player has the lowest score.
hasScore :: Int -> GameResult -> Bool
hasScore max_score (GameResult _ scores _) =
  finalScore loser >= max_score && finalScore start < finalScore contender
  where
    sorted = sortOn finalScore scores
    loser = last sorted
    start = head sorted
    contender = head (tail sorted) -- There should be at least two players

-- | Shuffle a deck then start the game, keep track of the score.
playDeck :: GameResult -> EitherIO GameError GameResult
playDeck (GameResult previous results players) = do
  deck <- liftIO shuffledDeck
  played <- playHand players deck
  return $ GameResult
    (played: previous)
    (calculateScore results (scores played))
    players

calculateScore :: [GameScore] -> [HandScore] -> [GameScore]
calculateScore scores played = zipWith toScore
  (sortOn gameId scores)
  (sortOn scoreId played)
  where
    toScore (GameScore p current) (HandScore _ score) =
      GameScore p (current + score)

-- | Distribute a deck to the players.
playHand :: [Player] -> [Card] -> EitherIO GameError HandResult
playHand players deck = do
  tricks <- startHand (zipWith Hand players hands)
  return $ HandResult tricks (calculateHandScores (tallyTricks players tricks))
  where
    nbPlayers = length players
    handSize = (length deck) `div` nbPlayers
    hands = deal handSize nbPlayers deck

-- | Find the starting player (has the Two of Clubs) then play the hands.
startHand
  :: [Hand]                     -- ^ Player hands
  -> EitherIO GameError [Trick]
  -- ^ The played out tricks for each player
startHand hands = playTricks order []
  where
    use :: Hand -> Player
    use = player
    start = use . head . filter ((two_clubs `elem`) . cards) $ hands
    order = rotateHands (playerId (start :: Player)) hands

-- | Rotate hands to move the specified player into the lead position
-- player order is tail of list goes first
rotateHands :: PlayerId -> [Hand] -> [Hand]
rotateHands pid hs =
  case findIndex ((pid ==) . handId) rhands of
    Just p -> spliceAndReverse (splitAt p rhands)
    _ -> error "unknown player"
    where
      rhands = reverse hs
      spliceAndReverse (a, b) = reverse $ b ++ a

-- | Play tricks for the given hands until no cards remaining in hands. Returns
-- the played out tricks or an error.
playTricks
  :: [Hand]                     -- ^ remaining hands for each player
  -> [Trick]                    -- ^ tricks played so far
  -> EitherIO GameError [Trick]
playTricks (Hand { cards=[] } : _) tricksSoFar = return tricksSoFar
playTricks hands tricksSoFar = do
  (trick, newHands) <- playTrick hands tricksSoFar
  playTricks newHands (trick: tricksSoFar)

-- | Play a trick, in case of an illegal play the guilty player is identified,
-- otherwise return played-out trick and the remaining hands, rotated so that
-- the winner is in lead position
playTrick
  :: [Hand]                     -- ^ hands in play order
  -> [Trick]                    -- ^ tricks played so far
  -> EitherIO GameError (Trick, [Hand])
playTrick hands tricksSoFar = do
  (trick, playerHands) <- foldr
    (playCardToTrick tricksSoFar) (return ([], [])) hands
  let rotated = rotateHands (winner (leadSuit trick) trick) playerHands
  return (trick, rotated)

-- | Choose and move card from hand to trick
playCardToTrick
  :: [Trick]                    -- ^ tricks played in previous rounds
  -> Hand                       -- ^ player hand and saved data
  -> EitherIO GameError (Trick, [Hand])
  -> EitherIO GameError (Trick, [Hand])
playCardToTrick tricksSoFar hand stateOfPlay = do
  (trick, playedHands) <- stateOfPlay
  (p, h) <- chooseCard hand tricksSoFar trick
  return (p: trick, h: playedHands)

-- | Call player's chooseCard function, with a timer.
chooseCard
  :: Hand                       -- ^ current hand
  -> [Trick]                    -- ^ previous tricks in hand
  -> Trick                      -- ^ current trick
  -> EitherIO GameError (Play, Hand)
chooseCard hand tricksSoFar trick = do
  (played, saved) <- EitherIO call
  card <- liftEither (validPlay played playerId handCards current before)
  return ((card, saved, playerId), hand{cards=delete card handCards})
  where
    (Hand Player{playerId,playFunc} handCards) = hand
    memory = getMemory playerId tricksSoFar
    cleaned = map (\(x, _, z) -> (x, z)) trick
    picked = playFunc playerId handCards cleaned memory
    before = (map . map) first tricksSoFar
    current = map first trick
    call = do
      -- Careful, these are microsecs
      played <- timeout 1000000 $ return $!! picked -- Will force evaluation
      let timed = case played of
            Nothing -> Left $ GameError TimeError playerId "took too long to play."
            Just c -> Right c
      return timed

getMemory :: PlayerId -> [Trick] -> Maybe ([(Card, PlayerId)], String)
getMemory _ [] = Nothing
getMemory pid (last_trick: _) = Just (played last_trick, memory last_trick)
  where
    played = map no_snd
    memory = second . head . filter ((== pid) . third)

-- | Count points for each trick, find the winning player, and tally.
tallyTricks
  :: [Player]                   -- ^ players
  -> [Trick]                    -- ^ played-out tricks
  -> [(PlayerId, Int)]          -- ^ how many points each player has won
tallyTricks players = Map.toList . Map.fromListWith (+) . (nil ++) . points
  where
    -- We need all players, even those who did not win a trick
    nil = map (\Player{playerId} -> (playerId, 0)) players
    -- Turn a list of tricks into a list of (winner, points per trick)
    points = map (\t -> (won t, count t))
    -- Count points in a trick
    count t = sum (map cardPoints (map first t))
    won trick = winner (leadSuit trick) trick

-- | If anyone has managed a /Shot for the Moon!/ every other player gets 26
-- points, otherwise convert to 'HandScore'.
calculateHandScores :: [(PlayerId, Int)] -> [HandScore]
calculateHandScores trickTally = case find ((== full_score) . snd) trickTally of
  Nothing -> map toHandScore trickTally
  Just (pid, _) -> map (toFullScore pid) trickTally
  where
    toFullScore won (pid, _) | won == pid = toHandScore (won, 0)
                             | otherwise = toHandScore (pid, full_score)

-- Convenience conversion functions.

toHandScore :: (PlayerId, Int) -> HandScore
toHandScore = HandScore <$> fst <*> snd

gameId :: GameScore -> PlayerId
gameId (GameScore Player{playerId} _) = playerId

scoreId :: HandScore -> PlayerId
scoreId (HandScore pid _) = pid

handId :: Hand -> PlayerId
handId (Hand Player{playerId} _) = playerId
