module ShutTheBox.Console where

import ShutTheBox.Types
import ShutTheBox.Pretty

main :: IO ()
main = do
  let pm = PointMarkers [False, True, True, True, True, False, True, True, False]
  putStrLn $ pretty pm
  putStrLn $ pretty $ Dice (Die3, Die6)
