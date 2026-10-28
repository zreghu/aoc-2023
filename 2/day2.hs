import Data.List
import Data.Char


main :: IO ()
main = do
    inp <- readFile "2/input.txt"
    putStrLn $ "Part 1 Cubes: " ++ show (day2 inp)
    putStrLn $ "Part 2 Cubes: " ++ show (part2 inp)
    return ()

data Set = Set 
    { r :: Int
    , g :: Int
    , b :: Int
    } deriving Show

data Game = Game Int [Set] deriving Show


data Color = Red | Green | Blue deriving Show
data Token = Alpha String | Digit String | Space | Colon | SemiColon deriving Show
data WordG  = GameW Int | ColorW Int Color | ColonW | SemiColonW deriving Show

lex:: String -> [Token]
lex t = reverse . map reverseT $ foldl' go [] t
    where reverseT (Alpha t') = Alpha $ reverse t'
          reverseT (Digit t') = Digit $ reverse t'
          reverseT t'         = t'
          go acc c
            | isSpace c = acc
            | isAlpha c = case acc of
                            []            -> [Alpha [c]]
                            Alpha c' : tl -> Alpha (c : c') : tl
                            _             -> Alpha [c] : acc
            | isDigit c = case acc of
                            []            -> [Digit [c]]
                            Digit d : tl  -> Digit (c: d) : tl
                            _             -> Digit [c] : acc
            | c == ':'  = Colon : acc
            | c == ';'  = SemiColon : acc
            | otherwise = acc

wordify :: [Token] -> [WordG]
wordify [] = []
wordify (Alpha "Game" : Digit d : tl)   = GameW (read d)        : wordify tl
wordify (Digit d : Alpha "red"   : tl)  = ColorW (read d) Red   : wordify tl
wordify (Digit d : Alpha "green" : tl)  = ColorW (read d) Green : wordify tl
wordify (Digit d : Alpha "blue"  : tl)  = ColorW (read d) Blue  : wordify tl
wordify (Colon : tl)                    = ColonW                : wordify tl
wordify (SemiColon : tl)                = SemiColonW            : wordify tl
wordify (_ : tl)                       = wordify tl

emptySet :: Set
emptySet = Set { r = 0, g = 0, b = 0 }

emptyGame :: Game
emptyGame = Game 0 [emptySet]

addColor :: Set -> WordG -> Set
addColor s (ColorW n Red)   = s { r = n }
addColor s (ColorW n Green) = s { g = n }
addColor s (ColorW n Blue)  = s { b = n }
addColor s _ = s

parse :: Game -> [WordG] -> Game
parse ga []                         = ga
parse (Game n s) (ColonW     : tl) = parse (Game n s) tl
parse (Game _ s) (GameW n'   : tl) = parse (Game n' s) tl
parse (Game n s) (SemiColonW : tl) = parse (Game n $ emptySet:s) tl
parse (Game n (s:sl)) (color : tl) = parse (Game n $ addColor s color : sl) tl
parse ga (_:tl)                    = parse ga tl

badSet :: Set -> Set -> Bool
badSet (Set { r = r1, g = g1, b = b1 }) (Set { r = r2, g = g2, b = b2 }) 
    = r1 < r2 || g1 < g2 || b1 < b2

badGame :: Set -> Game -> Bool
badGame s (Game _ sl) = any (badSet s) sl 

bar :: Set
bar = Set { r = 12, g = 13, b = 14 }

day2 :: String -> Int
day2 s = foldl' (\acc (Game n _) -> n + acc) 0 . filter (not . badGame bar) $ games s
    where games s' = map (parse emptyGame . wordify . Main.lex) $ lines s'

minSet :: [Set] -> Set
minSet sl = let min_r = maximum $ map (\Set { r=r' } -> r') sl in
            let min_g = maximum $ map (\Set { g=g' } -> g') sl in
            let min_b = maximum $ map (\Set { b=b' } -> b') sl in
            Set { r=min_r, g=min_g, b=min_b }

power :: Game -> Int
power (Game _ sl) = let Set {r=r',g=g',b=b'} = minSet sl in
    r' * g' * b'

part2 :: String -> Int
part2 s = sum . map power $ games s
    where games s' = map (parse emptyGame . wordify . Main.lex) $ lines s'

