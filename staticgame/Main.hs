module Main where

import Hearts.Play
import Hearts.Types
import Game
import EitherIO
import Logs
import safe qualified Player
import Control.Monad

-- This sets up a tournament with four instances of your player playing against
-- each other.  You can run different players against each other, but you'll
-- need to change the Module names of those players (don't forget to change the
-- module name back to "Player" when you submit your code)
players :: [Player]
players =
    [ newPlayer "4" Player.playCard Player.makeBid
    , newPlayer "3" Player.playCard Player.makeBid
    , newPlayer "2" Player.playCard Player.makeBid
    , newPlayer "1" Player.playCard Player.makeBid
    ]

main :: IO ()
main = do
  clearAllLogs players
  played <- runEitherIO $ playGame 10000 players
  case played of
    Right gr@(GameResult hr scores _) -> do
      forM_ (reverse hr) print
      putStrLn "=============="
      forM_ scores print
      writeGame gr
    Left e -> print e
