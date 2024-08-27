import System.Environment (getArgs)
import Parser
import Text.Parsec
import Data.List (nub, sort, foldl', union, (\\))
import qualified Data.Set as Set
import Data.Set (Set)

type Lights = Set (Int, Int)

extractCommand :: Either ParseError Command -> Command
extractCommand command = case command of
  Left err -> error $ "Parsing error: " ++ show err
  Right comm -> comm

countLightsLit :: Lights -> Int
countLightsLit = Set.size

changeLightsStatusList :: [(Int, Int)] -> Command -> [(Int, Int)]
changeLightsStatusList lights (TurnOn (x0, y0) (x1, y1)) = lights `union` ([(x, y) | x <- [x0..x1], y <- [y0..y1]])
changeLightsStatusList lights (TurnOff (x0, y0) (x1, y1)) = lights \\ [(x, y) | x <- [x0..x1], y <- [y0..y1]]
changeLightsStatusList lights (Toggle (x0, y0) (x1, y1)) =
    foldl' toggleLight lights [(x, y) | x <- [x0..x1], y <- [y0..y1]]
    where
        toggleLight acc (x, y) = if (x, y) `elem` acc then filter (/= (x, y)) acc else (x, y) : acc

changeLightsStatusSet :: Lights -> Command -> Lights
changeLightsStatusSet lights (TurnOn (x0, y0) (x1, y1)) = Set.union lights (Set.fromList [(x, y) | x <- [x0..x1], y <- [y0..y1]])
changeLightsStatusSet lights (TurnOff (x0, y0) (x1, y1)) = Set.difference lights (Set.fromList [(x, y) | x <- [x0..x1], y <- [y0..y1]])
changeLightsStatusSet lights (Toggle (x0, y0) (x1, y1)) =
    foldl' toggleLight lights [(x, y) | x <- [x0..x1], y <- [y0..y1]]
    where
        toggleLight acc (x, y) = if Set.member (x, y) acc then Set.delete (x, y) acc else Set.insert (x, y) acc

applyCommandsList lights commands = foldl changeLightsStatusList lights commands

applyCommandsSet :: Lights -> [Command] -> Lights
applyCommandsSet = foldl' changeLightsStatusSet

mainSet :: IO ()
mainSet = do
  content <- readFile "input.txt"
  let linesList = lines content
  let commands = map (extractCommand . parseCommand) linesList
  let lightsLit = applyCommandsSet Set.empty commands

  print $ length lightsLit

mainList :: IO ()
mainList = do
  content <- readFile "input.txt"
  let linesList = lines content
  let commands = map (extractCommand . parseCommand) linesList
  let lightsLit = applyCommandsList [] commands

  print $ length lightsLit

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["set"] -> mainSet
    ["list"] -> mainList
    _        -> putStrLn "Usage: program_name (set|list)"
