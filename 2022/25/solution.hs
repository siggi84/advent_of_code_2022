snafuToDec sn = res
  where
    sToN '0' = 0
    sToN '1' = 1
    sToN '2' = 2
    sToN '-' = -1
    sToN '=' = -2
    sToN _ = error "Unexpected symbol"
    res = sum $ zipWith (\e s -> sToN s * 5 ^ e) [0 ..] (reverse sn)

decToSnafu v =
  if v == 0
    then ""
    else res
  where
    qn = div v 5
    m = mod v 5
    res
      | m == 0 = decToSnafu qn ++ "0"
      | m == 1 = decToSnafu qn ++ "1"
      | m == 2 = decToSnafu qn ++ "2"
      | m == 3 = decToSnafu (qn + 1) ++ "-"
      | m == 4 = decToSnafu (qn + 1) ++ "="

part1 snafuNumbers = decToSnafu $ sum $ map snafuToDec snafuNumbers

main = do
  inputString <- readFile "input.dat"
  let parsedInput = lines inputString
  print (part1 parsedInput)
