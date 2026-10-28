import Data.List
-- import Debug.Trace
import Text.Printf
import Text.Parsec
import Text.Parsec.String

data Card = Card Int [Int] [Int] deriving Show

main :: IO ()
main = do
    inp <- readFile "4/input.txt"
    printf "Part1: %d\n" $ part1 (parseFile inp)
    printf "Part2: %d\n" $ part2 (parseFile inp)
    return ()

-- Part 1
points :: Card -> Int
points (Card _ w xs)
    | matches == 0 = 0
    | otherwise    = 2 ^ (matches - 1)
    where matches = length $ w `intersect` xs

part1 :: [Card] -> Int
part1 = sum . map points

-- Part 2
overlap :: Card -> Int
overlap (Card _ w xs) = length (w `intersect` xs)

-- Critique: either create a new type or use a record to manage the list
--           otherwise it's a bit unclear what should go into processCards
--           we don't just want any two int lists to go in 
--           so either alias the types or create a new type to input
processCards :: [Int] -> [Int] -> [Int]
processCards _ [] = [] 
processCards [] ns = ns
processCards (c:cs) (n:ns) =
    let ns' = addN c n ns in
    n : processCards cs ns'

addN :: Int -> Int -> [Int] -> [Int]
addN _ _ []     = []
addN 0 _ xs     = xs
addN i n (x:xs) = x + n : addN (i-1) n xs

part2 :: [Card] -> Int
part2 xs =
    let cards = map overlap xs in
    sum $ processCards cards (replicate (length xs) 1)

-- Parser
parseInput :: Parser [Card]
parseInput = line `endBy` endOfLine

whitespace :: Parser Char
whitespace = char ' ' <|> tab

number :: Parser Int
number = read <$> many1 digit

numbers :: Parser [Int]
numbers = number `sepEndBy` many1 whitespace

line :: Parser Card
line = Card <$ string "Card" <* spaces 
        <*> number <* char ':' <* spaces
        <*> numbers <* char '|' <* spaces
        <*> numbers

parseFile :: String -> [Card]
parseFile i = 
    let result = parse parseInput "" i in
    case result of
        Left  _ -> [] -- Parsing Failed
        Right x -> x
