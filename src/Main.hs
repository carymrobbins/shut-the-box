module Main where

import Control.Monad
import Data.Coerce
import Data.Maybe
import Data.Ratio
import Data.List (group, sort, subsequences)
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  undefined

newtype Score = Score Int
  deriving Show

newtype Roll = Roll Int
  deriving (Show, Eq)

newtype RollProbabilityMap = RollProbabilityMap [(Roll, Probability)]
  deriving Show

newtype Probability = Probability (Ratio Int)
  deriving Show

newtype PointMarkers = PointMarkers [Bool]
  deriving Show

data Move = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Show, Bounded, Enum)

newtype DecisionTree = DecisionTree [([Move], DecisionTree)]
  deriving Show

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

newPointMarkers :: PointMarkers
newPointMarkers = PointMarkers $ replicate 9 True

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
