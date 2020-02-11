-- | Write a report describing your design and strategy here.
-- Research performed when searching about Hearts strategies were from the sites
-- https://viphearts.com/blog/hearts-tips/
-- https://viphearts.com/blog/advanced-strategies-in-hearts/

-- PLAYING STRATEGY
-- Code follows a standard strategy consisting of a number of logical statements
-- with the aim of ensuring that the player never ends up taking Queen of Spades,
-- and the player minimizes his chances of taking a trick with hearts. The following
-- statements, which will be developed as functions, will help ensure this strategy.
-- At any trick = AAT
-- Memory in my game = [Card] which represents all cards played in the round so far

-- 1. AAT: If the player is deemed to win a trick, play the highest card possible
--       : Memory analyzes suits played and rankings and determines this
-- 2. AAT: Get rid of the highest ranking card whenever it is safe to do so
-- 3. AAT: Try to void diamonds or clubs as soon as possible
--       : so that we can get rid of unwanted Hearts, and High Spades
-- 4. AAT: If the player voids the leading suit,
--                  play the highest ranking point card (SQ -> HA -> HK...)
--                  , Otherwise play the highest ranking non-point card
-- 5. AAT: If leading, play the safest card with two options:
--     5.1 If it is highly unlikely for a suit to be voided, play the highest non-point rank card of that suit
--     5.2 If the player must place a suit that is highly likely to be voided, play the lowest rank card of that suit
--         if a higher rank card of the suit is still present. 

-- AAT After Breaking: Place the highest ranking point card whenever it is safe to do so
-- AAT After Breaking: At any trick with points involved, play the lowest rank leading suit
--                     if possible to avoid winning the trick

-- Queen of Spade Strategy - At any point in the game, the player ensures that
--      1. they will not win a trick where there is a chance the SQ will be played out
        --      this may result when a suit is voided, and a player decides to play their SQ
--      2. At any given chance, the player will play the SQ out to the winner of the trick
--      3. Never lead a trick with the SQ unless it is safe to do so
        --      other players have higher spades
--      4. Always keep low spades when the SQ has not been played yet
        --      during a spade trick

-- The following logical statements above are supplemented by memory. Whenever I
-- write 'safe to do so' above, this means via memory analysis of ALL cards played
-- so far in the round, and of the cards played so far in the current trick.


module Player (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
-- We can use convenience functions from Types
import Cards
import Hearts.Types
import Hearts.Rules

-- Get the suit of the card
getSuit :: Card -> Suit
getSuit (Card suit _) = suit

-- Get the rank of the card
getRank :: Card -> Rank
getRank (Card _ rank) = rank

-- Gets the leading suit from the current Trick
-- Leading suit is the first played card in a trick, which is the last card in the list
leadSuit :: [Card] -> Suit
leadSuit cards = getSuit $ last cards

-- gets the cards played in current trick, excluding the player ID's
-- Eta-reduced Function
getCards :: [(Card, PlayerId)] -> [Card]
getCards = map fst

-- Returns a list of cards which has the suit given
cardsOfSuit :: Suit -> [Card] -> [Card]
cardsOfSuit suit = filter (\card -> getSuit card == suit)

-- returns the maximum card in the list by its rank
maximumByRank :: [Card] -> Card
maximumByRank cardList = foldl (\card1 card2 -> if getRank card1 > getRank card2 then card1 else card2) (head cardList) cardList

-- returns the maximum card in the list by its rank
-- minimumByRank :: [Card] -> Card
-- minimumByRank cardList = foldl (\card1 card2 -> if getRank card1 > getRank card2 then card2 else card1) (head cardList) cardList

-- -- Returns a list of cards which does not have the suit given
notCardsOfSuit :: Suit -> [Card] -> [Card]
notCardsOfSuit suit = filter (\card -> getSuit card /= suit)

-- checks if there is an higher spade inside the trick
higherSpadeQueenInTrick :: [Card] -> [Card]
higherSpadeQueenInTrick = filter (\card -> getRank card > getRank queen_spades)

-- Define all Rules related to playing a card during a trick
-- 1. Leading Rule
-- check if player must lead first trick (place the two of clubs)
-- Eta-reduced Function
mustLeadFirst :: [Card] -> Bool
mustLeadFirst playerCards = length (filter (== two_clubs) playerCards) == 1

-- Function returns a list of player cards that have no points
noPointCards :: [Card] -> [Card]
noPointCards = filter (\card -> getSuit card /= Heart && card /= queen_spades)

-- Function returns a list of player cards that have points
pointCards :: [Card] -> [Card]
pointCards = filter (\card -> getSuit card == Heart || card == queen_spades)

-- Function which checks if hearts has already been played aka 'broken' in the current round
-- parameter: cardsPlayed is the memory in the game + cards in current trick - storing ALL cards played
heartsBroken :: [Card] -> Bool
heartsBroken cardsPlayed = not (null (cardsOfSuit Heart cardsPlayed))

-- Function which checks if Queen of Spade has already been played in the current round
-- parameter: cardsPlayed is the memory in the game + cards in current trick - storing ALL cards played
spadeQueenPlayed :: [Card] -> Bool
spadeQueenPlayed cardsPlayed = length (filter (== queen_spades) cardsPlayed) == 1

-- 2. Bleeding/Breaking Rule
-- Cannot lead with a heart in a trick unless only have hearts in hand
-- Function returns a list of playable cards
-- bleedingRule :: [Card] -> [Card]
-- bleedingRule playerCards = if length (cardsOfSuit Heart playerCards) == length playerCards
--                             then playerCards 
--                             else notCardsOfSuit Heart playerCards

-- 4. Breaking Rule
-- No player may start a trick with a heart before 
-- any has been played, unless they have no other choice
-- checked inside playCard function

-- Function which removes duplicate cards
removeDuplicateCards :: [Card] -> [Card]
removeDuplicateCards [] = []
removeDuplicateCards (x:xs)
    | x `elem` xs = removeDuplicateCards xs
    | otherwise = x : removeDuplicateCards xs

-- cardlist used for debugging
-- pointlist = [Card Spade Two, Card Heart Five, Card Spade Queen, Card Diamond Nine]
-- nopointlist = [Card Spade Six, Card Club King, Card Diamond Ace]
-- diamondorclublist = [Card Diamond Six, Card Club King, Card Diamond King, Card Diamond Ace]

-- Memory to analyse for current round - convert memory into [Card]
-- shows all the Card that has been played in previous rounds
analyzeMemory :: Maybe ([(Card, PlayerId)], String) -> [Card]
analyzeMemory Nothing = []
analyzeMemory (Just (lastTrick, memory)) = if memory == "Nothing"
                                           then getCards lastTrick
                                           else read memory ++ getCards lastTrick

-- Memory to return for next round
memoryForNextRound :: Maybe ([(Card, PlayerId)], String) -> String
-- -- first trick: if no cards played in previous trick, return "Nothing" as memory
memoryForNextRound Nothing = "Nothing"
-- second trick: (no memory, but have cards played in previous trick), then return cards played as memory
-- if at > 2 rounds, return the memory combined with cards played in previous trick
memoryForNextRound (Just (lastTrick, memory)) = if memory == "Nothing" 
                                                then show (getCards lastTrick)
                                                else show (read memory ++ getCards lastTrick)


-- determines the card to play on the first Trick
determineFirstTrickPlay :: [Card] -> [Card] -> Card
determineFirstTrickPlay currentTrick playerCards
    | leadFirst = two_clubs-- must lead first trick
    | not $ null leadSuitCards = maximum leadSuitCards -- play highest lead suit card since first trick has no points
    | not (null diamondCards) && not (null clubCards) = maximum diamondOrClub -- play highest card of either suit which has the least amount to void
    | not $ null diamondCards = maximum diamondCards
    | not $ null clubCards = maximum clubCards
    | not $ null spadeCards = maximum spadeCards -- if no choice but to place spade, place highest spade since first trick very unlikely anyone can play SQ
    | otherwise = maximum heartCards -- if no diamond, club, or spades, play the highest heart
    where
        leadFirst = mustLeadFirst playerCards
        leadSuitCards = cardsOfSuit (Player.leadSuit currentTrick) playerCards
        diamondCards = cardsOfSuit Diamond playerCards
        clubCards = cardsOfSuit Club playerCards
        diamondOrClub = if length diamondCards < length clubCards then diamondCards else clubCards
        spadeCards = cardsOfSuit Spade playerCards
        heartCards = cardsOfSuit Heart playerCards

-- Function returns a list of cards which is smaller to the card supplied
possibleLosingCards :: [Card] -> Card -> [Card]
possibleLosingCards playerCards smallCard = filter (< smallCard) playerCards


-- Determines the safest card the player can lead in the current trick
-- based around memory analysis of checking suits played in previous tricks
-- suit play preference will always be diamonds and clubs first
-- assume if 6 cards of a suit is still present = safe

-- if the suit is highly likely to be voided by another player, but we know that
-- other players has a higher rank card of the suit, then play the highest rank suit that guarantees a loss

-- if the suit is highly likely to not be voided by anyone, then play the highest rank card of the suit
safeLead :: [Card] -> [Card] -> Card
safeLead allCardsPlayed playerCards
    | playHighDiamond = maximum diamondCards
    | playSafeDiamond = maximum safeDiamonds
    | playHighClub = maximum clubCards
    | playSafeClub = maximum safeClubs

    -- reach this point means that the player either has no diamonds or clubs, or has diamonds or clubs that are highly likely to win a trick and, receive cards of other suits (such as SQ!!!)
    -- Hence, we do not want to play diamonds or clubs that risks receiving SQ, or hearts
    -- Consider Spades and Hearts now
    -- For Spades, check if SQ has been played
    -- For Hearts, check if hearts has been broken out

    | sqPlayed && playHighSpade = maximum spadeCards
    | not sqPlayed && playHighSpade && not (null playSafeAgainstQueen) = maximum playSafeAgainstQueen
    | playSafeSpade = maximum safeSpades
    | not (null spadeCards) = minimum spadeCards
    | heartIsBroken && playSafeHeart = maximum safeHearts
    | not heartIsBroken && not (null (notCardsOfSuit Heart playerCards)) = maximum (notCardsOfSuit Heart playerCards)
    
    -- reach this point means that the player has no safe cards to lead, in this case, just accept the win and play the highest card he has
    | otherwise = if not (null (noPointCards playerCards)) then maximumByRank (noPointCards playerCards) else maximumByRank (pointCards playerCards)
    where
        diamondCards = cardsOfSuit Diamond playerCards
        diamondsLeft = removeDuplicateCards ((Card <$> [Diamond] <*> [Two ..]) ++ cardsOfSuit Diamond allCardsPlayed)
        playHighDiamond = not (null diamondCards) && length diamondsLeft >= 6 -- safe to play the highest diamond when other players still have diamonds
        safeDiamonds = possibleLosingCards diamondCards (minimum diamondsLeft)
        playSafeDiamond = not (null diamondCards) && not (null diamondsLeft) && not (null safeDiamonds) -- safe to play a low diamond to lose the trick

        -- repeat same process for Clubs
        clubCards = cardsOfSuit Club playerCards
        clubsLeft = removeDuplicateCards ((Card <$> [Club] <*> [Two ..]) ++ clubCards ++ cardsOfSuit Club allCardsPlayed)
        playHighClub = not (null clubCards) && length clubsLeft >= 6
        safeClubs = possibleLosingCards clubCards (minimum clubsLeft)
        playSafeClub = not (null clubCards) && not (null clubsLeft) && not (null safeClubs)

        heartIsBroken = heartsBroken allCardsPlayed
        sqPlayed = spadeQueenPlayed allCardsPlayed

        spadeCards = cardsOfSuit Spade playerCards
        spadesLeft = removeDuplicateCards ((Card <$> [Spade] <*> [Two ..]) ++ spadeCards ++ cardsOfSuit Spade allCardsPlayed)
        playHighSpade = not (null spadeCards) && length spadesLeft >= 6
        playSafeAgainstQueen = possibleLosingCards spadeCards (Card Spade Queen)-- play the largest spade card that is smaller than the queen of spade
        safeSpades = possibleLosingCards spadeCards (minimum spadesLeft)
        playSafeSpade = sqPlayed && not (null spadeCards) && not (null spadesLeft) && not (null safeSpades)
        heartCards = cardsOfSuit Heart playerCards
        heartsLeft = removeDuplicateCards ((Card <$> [Heart] <*> [Two ..]) ++ heartCards ++ cardsOfSuit Heart allCardsPlayed)
        safeHearts = possibleLosingCards heartCards (minimum heartsLeft)
        playSafeHeart = not (null heartCards) && not (null heartsLeft) && not (null safeHearts)

-- Determines the safest card the player can follow up with in the current trick
-- based around memory analysis of checking suits played in previous tricks
-- If must renegade, then renegade in a safe manner
-- If cannot renegade, aka must break, place the highest point card we have (SQ -> HA -> HK...), 
--                                     otherwise, highest non point card

safeFollowUp :: [Card] -> [Card] -> [Card] -> Suit -> Card
safeFollowUp memoryCards curTrick playerCards leadingSuit

    -- first ensure that the lead suit is not spades: if its spades, ensure that the SQ is already played before playing high spades
    | not sqInTrick && sqPlayed && playHighLead = maximum leadCards
    | leadingSuit == Spade && weHaveSpades && not (null getRidOfQueenSpade) = Card Spade Queen
    | not sqPlayed && playHighLead && leadingSuit == Spade && not (null playSafeAgainstQueen) = maximum playSafeAgainstQueen
    | not sqPlayed && sqInTrick && playSafeLead = maximum safeLeads
    | playHighLead = maximum leadCards
    | playSafeLead = maximum safeLeads
    -- has lead suit, and highly likely to win the trick, then just place highest lead suit card
    | not (null leadCards) = maximum leadCards
    -- lead cards voided, think about the highest rank point card to place
    | weHaveSpades = Card Spade Queen
    | heartIsBroken && not (null heartCards) = maximum heartCards
    -- hearts not broken and we have hearts, then put highest heart
    | not heartIsBroken && not (null heartCards) = maximum heartCards
    -- no point cards, just place highest non-point card then
    -- hearts is not broken but no more point cards, just play highest non point card
    | otherwise = maximumByRank npCards
    where
        sqPlayed = spadeQueenPlayed memoryCards
        sqInTrick = spadeQueenPlayed curTrick
        spadeCards = cardsOfSuit Spade playerCards
        heartIsBroken = heartsBroken (memoryCards ++ curTrick)
        heartCards = cardsOfSuit Heart playerCards
        getRidOfQueenSpade = higherSpadeQueenInTrick curTrick
        playSafeAgainstQueen = possibleLosingCards spadeCards (Card Spade Queen) -- play the largest spade card that is smaller than the queen of spade
        leadCards = cardsOfSuit leadingSuit playerCards
        leadCardsLeft = removeDuplicateCards ((Card <$> [leadingSuit] <*> [Two ..]) ++ leadCards ++ cardsOfSuit leadingSuit (memoryCards ++ curTrick))
        playHighLead = not (null leadCards) && length leadCardsLeft >= 6 -- safe to play the highest lead when other players still have the suit
        safeLeads = possibleLosingCards leadCards (minimum leadCardsLeft)
        playSafeLead = not (null leadCards) && not (null leadCardsLeft) && not (null safeLeads) -- safe to play a low lead to lose the trick
        pCards = pointCards playerCards
        npCards = noPointCards playerCards
        weHaveSpades = length (filter (== queen_spades) pCards) == 1
        

-- determine cards to play based on memory analysis
determineNTrickPlay :: [Card] -> [Card] -> [Card] -> Card
determineNTrickPlay memoryCards curTrick playerCards
    | mustLead = safeLead memoryCards playerCards -- if must lead, play the 'safest' card to lead
    | otherwise = safeFollowUp memoryCards curTrick playerCards (Player.leadSuit curTrick)
    where
        mustLead = null curTrick

-- play a card based on the strategy proposed on the header
-- returns a memory string based on incremental cards played per trick
playCard :: PlayFunc
-- playCard = error "You need to implement the playCard function."
playCard _ playerCards currentTrick memoryState
    | null memoryCards = (determineFirstTrickPlay trickCards playerCards, nextMemory)  -- First Trick Play
    | otherwise = (determineNTrickPlay memoryCards trickCards playerCards, nextMemory)  -- Future Trick Play
    where
        trickCards = getCards currentTrick
        memoryCards = analyzeMemory memoryState
        nextMemory = memoryForNextRound memoryState


-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined