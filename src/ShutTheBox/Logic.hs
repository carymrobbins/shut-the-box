module ShutTheBox.Logic where

import ShutTheBox.Types

import Control.Monad
import Data.Coerce
import Data.List
import Data.Maybe
import Data.Ratio

allMoves :: [Move]
allMoves = [minBound..maxBound]

moveValue :: Move -> Int
moveValue m = fromEnum m + 1

buildDecisionTree :: Roll -> PointMarkers -> DecisionTree
buildDecisionTree = undefined -- roll (PointMarkers markers)

possibleMoves :: Roll -> PointMarkers -> [Move]
possibleMoves (Roll roll) pm =
  undefined
  where
  moveChoices = do
    xs <- subsequences $ availableMarkers pm
    guard $ sumWith moveValue xs == roll
    return xs

sumWith :: (Num b, Foldable t) => (a -> b) -> t a -> b
sumWith f = foldl (\acc x -> acc + f x) 0

availableMarkers :: PointMarkers -> [Move]
availableMarkers (PointMarkers ms) = do
  (move, True) <- zip allMoves ms
  return move

computeScore :: PointMarkers -> Score
computeScore pm = Score $ foldl (\acc x -> acc * 10 + moveValue x) 0 $ availableMarkers pm

diceRolls :: RollProbabilityMap
diceRolls =
  coerce $ map (\xs -> (head xs, length xs % numRolls)) $ group $ sort allRolls
  where
  allRolls = (+) <$> [1..6] <*> [1..6] :: [Int]
  numRolls = length allRolls

getProbability :: Roll -> Probability
getProbability roll =
  fromMaybe
    (error $ "Could not find probability for " ++ show roll)
    $ lookup roll $ coerce diceRolls
