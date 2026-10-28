import Data.Char
-- import Debug.Trace

main :: IO ()
main = do
    inp <- readFile "3/input.txt"
    putStrLn $ "Part 1 Computer: " ++ show (sumLines 0 "" $ lines inp)
    putStrLn $ "Part 2 Computer: " ++ show (sumLinesPart2 0 "" $ lines inp)
    return ()

getIndices :: (Char -> Bool) -> String -> [(Int, Char)]
getIndices b s = filter (\(_, c) -> b c) $ zip [0, 1..] s

isSymbol' :: Char -> Bool
isSymbol' '.' = False
isSymbol' c = not (isDigit c)


isAdjacent :: [Int] -> (Int, Int) -> Bool
isAdjacent [] _ = False
isAdjacent (hd:tl) b@(s,e) = (hd >= s - 1 && hd <= e + 1) || isAdjacent tl b

getNumbers :: String -> [((Int, Int), String)]
getNumbers s = foldl go [] $ getIndices isDigit s
    where go [] (i, c) = [((i, i), [c])]
          go (hd@((start,e),str):tl) (i,c) = 
            if e + 1 == i then ((start, i), c:str) : tl
            else ((i, i), [c]) : hd : tl

checkAdjacent :: [Int] -> [Int] -> [Int] -> [((Int, Int), String)] -> [((Int, Int), String)]
checkAdjacent before after during = filter adj
    where adj x = isAdjacent before (fst x) || isAdjacent after (fst x) || isAdjacent during (fst x)


sumNums :: [((Int, Int), String)] -> Int
sumNums = sum . map (read . reverse . snd)

sumLines :: Int -> String -> [String] -> Int
sumLines s before (l:after:tl) = 
    let b' = map fst $ getIndices isSymbol' before in
    let a' = map fst $ getIndices isSymbol' after in
    let d' = map fst $ getIndices isSymbol' l in
    let n' = getNumbers l in
    let s' = sumNums $ checkAdjacent b' a' d' n' in
    sumLines (s+s') l (after:tl)

sumLines s before (l:_) = 
    let b' = map fst $ getIndices isSymbol' before in
    let d' = map fst $ getIndices isSymbol' l in
    let n' = getNumbers l in
    let s' = sumNums $ checkAdjacent b' [] d' n' in
    s + s'

sumLines s _ _ = s

possiblyGear :: Char -> Bool
possiblyGear '*' = True
possiblyGear _   = False

checkAdjacentGear :: Int -> ((Int, Int), String) -> Bool
checkAdjacentGear g ((s,e),_) = s - 1 <= g && g <= e + 1

multiplyAdj :: [((Int, Int), String)] -> Int
multiplyAdj = product . map (read . reverse . snd)

sumLinesPart2 :: Int -> String -> [String] -> Int
sumLinesPart2 s before (l:after:tl) = 
    let b' = getNumbers before in 
    let a' = getNumbers after in
    let d' = getNumbers l in
    let gears = map fst $ getIndices possiblyGear l in
    let mergeAdjacent g = filter (checkAdjacentGear g) b' 
                            ++ filter (checkAdjacentGear g) a' 
                            ++ filter (checkAdjacentGear g) d' in
    let filtered = filter (\x -> length x == 2) . map mergeAdjacent $ gears in
    let s' = sum . map multiplyAdj $ filtered in
    sumLinesPart2 (s+s') l (after:tl)

sumLinesPart2 s before (l:_) = 
    let b' = getNumbers before in 
    let d' = getNumbers l in
    let gears = map fst $ getIndices possiblyGear l in
    let mergeAdjacent g = filter (checkAdjacentGear g) b' ++ filter (checkAdjacentGear g) d' in
    let filtered = filter (\x -> length x == 2) . map mergeAdjacent $ gears in
    let s' = sum . map multiplyAdj $ filtered in
    (s+s')

sumLinesPart2 s _ _ = s


