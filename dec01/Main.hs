{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

part1 = show . sum . map (\case '(' -> 1; ')' -> -1; _ -> 0)

part2 =
  show
    . length
    . takeWhile (>= 0)
    . scanl (+) 0
    . map (\case '(' -> 1; ')' -> -1; _ -> 0)

main = interact (\s -> unlines [part1 s, part2 s])
