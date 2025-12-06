{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Set qualified as Set

newtype Pos = Pos (Int, Int) deriving (Eq, Ord, Show)

Pos (x, y) +. Pos (z, w) = Pos (x + z, y + w)
Pos (x, y) -. Pos (z, w) = Pos (x - z, y - w)

move = \case
  'v' -> Pos (0, -1)
  '^' -> Pos (0, 1)
  '<' -> Pos (-1, 0)
  '>' -> Pos (1, 0)
  '\n' -> Pos (0, 0)

part1 =
  show
    . Set.size
    . Set.fromList
    . scanl (+.) (Pos (0, 0))
    . map move

takeOdd = \case
  (x:_:xs) -> x:takeOdd xs
  _ -> []

takeEven = takeOdd . drop 1

part2 input =
  let santa =
        scanl (+.) (Pos (0, 0))
          . map move
          . takeEven
          $ input
      robo =
        scanl (+.) (Pos (0, 0))
          . map move
          . takeOdd
          $ input
   in show . Set.size . Set.fromList $ santa ++ robo

main = interact (\s -> unlines [part1 s, part2 s])
