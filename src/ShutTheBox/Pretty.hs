module ShutTheBox.Pretty where

import ShutTheBox.Types

import Data.Monoid
import Data.List (intercalate, intersperse)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M

class Pretty a where
  pretty :: a -> String

-- | Merges strings side-by-side for pretty printing.
-- Inefficient, but does the job for now.
concatColumns :: [String] -> String
concatColumns xs = loop [] $ map lines xs
  where
  loop :: [String] -> [[String]] -> String
  loop acc [] = intercalate "\n" acc
  loop acc (slice:slices) = loop merged slices
    where
    merged = zipWith (<>) acc' slice'

    acc' =
      if length acc < length slice then
        acc <> replicate (length slice - length acc) ""
      else
        acc

    slice' = map pad $
      if length slice < length acc then
        slice <> replicate (length acc - length slice) ""
      else
        slice

    pad s = s <> replicate (maxLen - length s) ' '

    maxLen = case slice of
      [] -> 0
      s -> maximum $ map length s

instance Pretty Dice where
  pretty (Dice (d1, d2)) = concatColumns [pretty d1, pretty d2]

instance Pretty Die where
  pretty d = fromMaybe
    (error "Key missing from map: " <> show d)
    $ M.lookup d prettyDieMap

prettyDieMap :: Map Die String
prettyDieMap = M.fromList
  [ entry Die1
      [ "┌───────┐"
      , "│       │"
      , "│   ●   │"
      , "│       │"
      , "└───────┘"
      ]
  , entry Die2
      [ "┌───────┐"
      , "│ ●     │"
      , "│       │"
      , "│     ● │"
      , "└───────┘"
      ]
  , entry Die3
      [ "┌───────┐"
      , "│ ●     │"
      , "│   ●   │"
      , "│     ● │"
      , "└───────┘"
      ]
  , entry Die4
      [ "┌───────┐"
      , "│ ●   ● │"
      , "│       │"
      , "│ ●   ● │"
      , "└───────┘"
      ]
  , entry Die5
      [ "┌───────┐"
      , "│ ●   ● │"
      , "│   ●   │"
      , "│ ●   ● │"
      , "└───────┘"
      ]
  , entry Die6
      [ "┌───────┐"
      , "│ ●   ● │"
      , "│ ●   ● │"
      , "│ ●   ● │"
      , "└───────┘"
      ]
  ]
  where
  entry :: Die -> [String] -> (Die, String)
  entry d xs = (d, intercalate "\n" xs)

instance Pretty PointMarkers where
  pretty (PointMarkers xs) =
    concatColumns $ intersperse prettyPointMarkerBar tiles
    where
    f b i = if b then prettyPointMarkerTileUp i else prettyPointMarkerTileDown
    tiles = zipWith f xs [(1 :: Int)..]

prettyPointMarkerTileUp :: Int -> String
prettyPointMarkerTileUp n = intercalate "\n"
  [ "┌─────┐"
  , "│  " <> pad <> "│"
  , "│     │"
  , "│     │"
  , "└─────┘"
  ]
  where
  pad = take 3 $ show n <> "  "

prettyPointMarkerTileDown :: String
prettyPointMarkerTileDown = intercalate "\n"
  [ ""
  , ""
  , "┌─────┐"
  , "│     │"
  , "│     │"
  , "│     │"
  , "└─────┘"
  ]

prettyPointMarkerBar :: String
prettyPointMarkerBar = intercalate "\n"
  [ ""
  , ""
  , ""
  , "══"
  , ""
  ]
