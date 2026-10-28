-- import Debug.Trace
import Data.Attoparsec.Text
import Data.List
import Data.Maybe
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

main :: IO ()
main = do
    inp <- TIO.readFile "5/input.txt"
    print $ "Part1: " ++ show (part1 $ parseInput inp)
    print $ "Part2: " ++ show (part2 $ parseInput inp)
    return ()


processRange :: RangeP -> Int -> Int
processRange (RangeP dst src r) i = if src <= i && i < src + r then (i - src) + dst else i

mapNumber :: MapP -> Int -> Int
mapNumber MapP{..} i = foldr (\x acc -> if acc/=i then acc else processRange x i) i ranges

findMap :: AlmanacP -> String -> Maybe MapP
findMap AlmanacP{..} src = find (\MapP{direction=(DirectionP x _)} -> x == src) maps

followAlmanac :: AlmanacP -> String -> String -> Int -> Maybe Int
followAlmanac a src dst i
    | src == dst = Just i
    | otherwise  = let m = findMap a src in 
                   case m of
                       Nothing -> Nothing
                       Just m' -> let i'   = mapNumber m' i in
                                  let DirectionP _ src' = direction m' in
                                  followAlmanac a src' dst i'

part1 :: AlmanacP -> Int
part1 a@AlmanacP{..} = minimum $ mapMaybe (followAlmanac a "seed" "location") seeds

expandSeeds :: Seeds -> Seeds
expandSeeds []               = []
expandSeeds [_]              = []
expandSeeds (start:range:xs) = [start..start+range-1] ++ expandSeeds xs

-- TODO: Direct is too much processing, use Ranges
part2 :: AlmanacP -> Int
part2 a@AlmanacP{..} = minimum $ mapMaybe (followAlmanac a "seed" "location") (expandSeeds seeds)

-- Parser
data RangeP = RangeP Int Int Int deriving Show
data DirectionP = DirectionP String String deriving Show
data MapP = MapP { direction :: DirectionP, ranges :: [RangeP] } deriving Show
type Seeds = [Int]
data AlmanacP = AlmanacP
    { seeds :: Seeds
    , maps  :: [MapP]
    } deriving Show

seedsP :: Parser Seeds
seedsP = decimal `sepBy` skipSpace

rangeP :: Parser RangeP
rangeP = RangeP <$> decimal <* skipSpace <*> decimal <* skipSpace <*> decimal

directionP :: Parser DirectionP
directionP = DirectionP <$> many1 letter <* "-to-" <*> many1 letter

mapP :: Parser MapP
mapP = MapP <$> directionP <* " map:" <* endOfLine <*>
            rangeP `sepBy` skipSpace

almanacP :: Parser AlmanacP
almanacP = AlmanacP <$ "seeds: " <*> seedsP <* skipSpace
                <*> mapP `sepBy` skipSpace

parseInput :: T.Text -> AlmanacP
parseInput inp = 
    case parseOnly almanacP inp of
        Left  _ -> AlmanacP { seeds=[], maps=[] }
        Right r -> r

