-- | Hearts-specific rules
module Hearts.Rules where

import Deck
import Cards
import Game
import Hearts.Types
import Data.List
import EitherIO

-- Orphan instances, booh

instance Show HandResult where
  show (HandResult tricks scores) = unlines $
    map showTrick (reverse tricks) ++ map show scores
    where
      -- Remove the players' saved data from the tricks, display the winner,
      -- points won, and finish with the final score.
      showTrick cards = show (reverse (map (\(x, _, y) -> (x, y)) cards)) ++
        ", winner: " ++ winner (leadSuit cards) cards ++ ", points: " ++
        (show . sum) (map cardPoints (map first cards))

-- Some constants
two_clubs :: Card
two_clubs = Card Club Two

queen_spades :: Card
queen_spades = Card Spade Queen

full_score :: Int
full_score = 26

-- | The scoring in Hearts is:
--
--  - 1 point per Heart;
--  - 13 for the queen of Spades.
cardPoints :: Card -> Int
cardPoints (Card Spade Queen) = 13
cardPoints (Card Heart _) = 1
cardPoints _ = 0

-- | Winner of a given trick: player with the highest card in the given suit.
winner :: Suit -> Trick -> PlayerId
winner lead = third . head . reverse . (sortOn first) . filter (inSuit lead)

inSuit :: Suit -> Play -> Bool
inSuit suit ((Card s _), _, _) = s == suit

-- There is no bidding in Hearts.
dealAndBid :: Int -> [Player] -> [Card] -> EitherIO PlayerError [Hand]
dealAndBid n players deck = return $ zipWith Hand players hands
  where
    hands = deal n (length players) deck

leadSuit :: Trick -> Suit
leadSuit cards = suit
  where
    (Card suit _, _, _) = last cards

-- | Tell if a player renegs (does not follow suit)
reneging
  :: Card                       -- ^ Card played
  -> [Card]                     -- ^ Cards in hand
  -> Suit                       -- ^ Led suit
  -> [Card]                     -- ^ Current trick
  -> Bool
reneging _ _ _ [] = False
reneging (Card suit _) hand ledSuit _ =
  suit /= ledSuit && any followSuit hand
  where
    followSuit (Card s _) = s == ledSuit

-- | Verify that the chosen card is a valid play:
--
--  - Has to follow suit (reneging).
--  - Cannot lead with a Heart before another Heart was played (breaking).
--  - Has to play a card in hand (invalid).
--  - Cannot play point cards in the first round (bleeding).
validPlay
  :: Card                       -- ^ Card played
  -> PlayerId                   -- ^ Player
  -> [Card]                     -- ^ Hand
  -> [Card]                     -- ^ Previous cards in trick
  -> [[Card]]                   -- ^ Cards in previous tricks
  -> Either GameError Card
validPlay card playerId handCards trick previous
  | null previous && null trick && card /= two_clubs = leadError
  | null previous && (suit == Heart || card == queen_spades) &&
    not all_points = heartsError
  | null trick && suit == Heart && length broken == 0 &&
    not all_hearts = brokenError
  | reneging card handCards led_suit trick = renegError
  | card `notElem` handCards = invalidCardError
  | otherwise = Right card
  where
    cardSuit (Card s _) = s
    suit = cardSuit card
    led_suit = cardSuit (last trick)
    isHeart = (== Heart) . cardSuit
    broken = filter isHeart (concat previous)
    all_hearts = all isHeart handCards
    all_points = all ((> 0) . cardPoints) handCards
    makeError err reason = Left $ GameError err playerId reason
    -- Errors
    renegError = makeError RenegError $ "Didn't follow led suit: " ++
      show led_suit ++ "; in: " ++ show trick ++ "; with: " ++
      show handCards
    invalidCardError = makeError InvalidCardError $
        "Card played wasn't in player's hand: " ++ show card ++
        "Hand: " ++ show handCards
    leadError = makeError LeadError $ "Did not lead with Two of Clubs: " ++
      show card ++ "; with: " ++ show handCards
    brokenError = makeError BrokenError $
      "Played a Heart before the Hearts were broken: " ++
      (show $ previous) ++ "; with: " ++ show handCards
    heartsError = makeError BleedError $
      "Played a point card in the first round: " ++ show card
