module ShutTheBox.Types where

import Data.Ratio

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

newPointMarkers :: PointMarkers
newPointMarkers = PointMarkers $ replicate 9 True

data Move = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Show, Bounded, Enum)

newtype DecisionTree = DecisionTree [([Move], DecisionTree)]
  deriving Show

data Die = Die1 | Die2 | Die3 | Die4 | Die5 | Die6
  deriving (Show, Bounded, Enum, Eq, Ord)

newtype Dice = Dice (Die, Die)
  deriving Show
