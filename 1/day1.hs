import Data.Char
import Data.List
import Control.Applicative

main :: IO ()
main = do
   inp <- readFile "1/input.txt"
   putStrLn $ "Part 1 Calibration: " ++ show (getCalibrationSumPart1 inp)
   putStrLn $ "Part 2 Calibration " ++ show (getCalibrationSumPart2 inp)
   return ()

getFirstDigit :: String -> Maybe Int
getFirstDigit s = digitToInt <$> find isDigit s

getLastDigit :: String -> Maybe Int
getLastDigit s = digitToInt <$> find isDigit (reverse s)

getLineNumber :: String -> Maybe Int
getLineNumber s = 
    let f = getFirstDigit s in
    let l = getLastDigit s in
    case (f,l) of
        (Just a, Just b) -> Just $ a * 10 + b
        (_, _)           -> Nothing

getCalibration :: String -> Int
getCalibration s = read [head digits, last digits]
    where digits = filter isDigit s

-- Using Applicative function liftA2 to apply inside the maybe
sumMaybeInt :: [Maybe Int] -> Maybe Int
sumMaybeInt = foldr (liftA2 (+)) (Just 0)

getCalibrationSumMaybe :: String -> Maybe Int
getCalibrationSumMaybe s = sumMaybeInt $ map getLineNumber (lines s)

getCalibrationSumPart1 :: String -> Int
getCalibrationSumPart1 s = sum $ map getCalibration (lines s)

getCalibrationSumPart2 :: String -> Int
getCalibrationSumPart2 s = sum $ map (getCalibration . replaceNums) (lines s)

-- Solution from Dr. Neil Smith 
-- Notes: tails is the important, heavy lifting function here. By using it, 
-- we can get every suffix, and then apply the go function accordingly to
-- replace the number words to actual digits. Combine back together with a
-- fold of just digits. I think tails will be a common pattern seen when
-- trying to string parse. Another note is that this strategy only works
-- because the digits as words are prefix-free.
replaceNums :: String -> String
replaceNums s = reverse $ foldl' go "" $ tails s 
    where go acc [] = acc
          go acc xs 
            | "one"   `isPrefixOf` xs = '1' : acc
            | "two"   `isPrefixOf` xs = '2' : acc
            | "three" `isPrefixOf` xs = '3' : acc
            | "four"  `isPrefixOf` xs = '4' : acc
            | "five"  `isPrefixOf` xs = '5' : acc
            | "six"   `isPrefixOf` xs = '6' : acc
            | "seven" `isPrefixOf` xs = '7' : acc
            | "eight" `isPrefixOf` xs = '8' : acc
            | "nine"  `isPrefixOf` xs = '9' : acc
            | isDigit (head xs) = head xs : acc
            | otherwise = acc
