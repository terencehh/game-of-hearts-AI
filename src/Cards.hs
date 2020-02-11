-- | Implement the card-related types and helper functions
module Cards where

import Text.Read
import Data.Char

import Control.DeepSeq

-- | The four base suits
data Suit = Spade | Club | Diamond | Heart
  deriving (Eq, Ord, Enum, Bounded)

instance Show Suit where
  show Spade = "S"
  show Club = "C"
  show Diamond = "D"
  show Heart = "H"

instance Read Suit where
  readsPrec _ [] = []
  readsPrec _ (s: str) = case s of
    'S' -> [(Spade, str)]
    'C' -> [(Club, str)]
    'D' -> [(Diamond, str)]
    'H' -> [(Heart, str)]
    _ -> []

-- | The thirteen ranks
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Bounded)

instance Show Rank where
  show Ace = "A"
  show King = "K"
  show Queen = "Q"
  show Jack = "J"
  show rank = show $ fromEnum rank + 2

instance Read Rank where
  readsPrec _ [] = []
  readsPrec _ (s: str) = case s of
      'A' -> [(Ace, str)]
      'K' -> [(King, str)]
      'Q' -> [(Queen, str)]
      'J' -> [(Jack, str)]
      _   -> readNum
    where
      readNum
        | s >= '2' && s <= '9' = [(toEnum (ord s - ord '2') :: Rank, str)]
        | s == '1' = readZero str -- Read a following 0
        | otherwise = []
      readZero ('0': rest) = [(Ten, rest)]
      readZero _ = []

-- | A Card is a suit and a rank
data Card = Card Suit Rank
  deriving (Eq)

instance Show Card where
  show (Card suit rank) = show suit ++ show rank

instance NFData Card where
  rnf = rwhnf

instance Read Card where
  readsPrec _ str = do
     (s, rest) <- reads str
     (r, end) <- reads rest
     return (Card s r, end)

instance Ord Card where
  compare (Card s1 r1) (Card s2 r2) | s1 == s2 = compare r1 r2
                                    | otherwise = compare s1 s2
