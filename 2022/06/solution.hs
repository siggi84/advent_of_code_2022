import Data.List

markerLocation :: Int -> Int -> String -> Int

markerLocation n i "" = error "No marker found"
markerLocation n i s 
  | length (nub (take n s)) /= n = markerLocation n (i+1) (tail s)
  | otherwise                    = i + n

main = do
  input <- readFile "input.dat"
  let buffer = lines input !! 0
  print (markerLocation 4 0 buffer)
  print (markerLocation 14 0 buffer)
