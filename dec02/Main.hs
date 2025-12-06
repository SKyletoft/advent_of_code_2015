{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List
import Data.List.Split

part1 =
  show
    . sum
    . map
      ( ( \[l, w, h] ->
            let x = l * w
                y = w * h
                z = h * l
             in 2 * (x + y + z) + minimum [x, y, z]
        )
          . map (read :: String -> Int)
          . splitOn "x"
      )
    . lines

part2 =
  show
    . sum
    . map
      ( ( \sides ->
          let around = (*2) . sum . take 2 . sort $ sides
              bow = product sides
           in around + bow
        )
          . map (read :: String -> Int)
          . splitOn "x"
      )
    . lines

main = interact (\s -> unlines [part1 s, part2 s])
