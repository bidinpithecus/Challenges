import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Parser
import System.Environment (getArgs)
import Text.Parsec

type LightsSet = Set.Set (Int, Int)

type LightsMap = Map.Map (Int, Int) Int

extractCommand :: Either ParseError Command -> Command
extractCommand command = case command of
  Left err -> error $ "Parsing error: " ++ show err
  Right comm -> comm

changeLightsStatusSet :: LightsSet -> Command -> LightsSet
changeLightsStatusSet lights (TurnOn (x0, y0) (x1, y1)) = Set.union lights (Set.fromList [(x, y) | x <- [x0 .. x1], y <- [y0 .. y1]])
changeLightsStatusSet lights (TurnOff (x0, y0) (x1, y1)) = Set.difference lights (Set.fromList [(x, y) | x <- [x0 .. x1], y <- [y0 .. y1]])
changeLightsStatusSet lights (Toggle (x0, y0) (x1, y1)) =
  List.foldl' toggleLight lights [(x, y) | x <- [x0 .. x1], y <- [y0 .. y1]]
  where
    toggleLight acc (x, y) = if Set.member (x, y) acc then Set.delete (x, y) acc else Set.insert (x, y) acc

mapUnionWithCount :: LightsMap -> LightsMap -> LightsMap
mapUnionWithCount = Map.unionWith (+)

controlBrightness :: LightsMap -> LightsMap
controlBrightness = Map.map (`max` 0)

countBrightness :: LightsMap -> Int
countBrightness = Map.foldl (+) 0

changeLightsStatusMap :: LightsMap -> Command -> LightsMap
changeLightsStatusMap lights (TurnOn (x0, y0) (x1, y1)) =
  mapUnionWithCount lights newLights
  where
    newLights = Map.fromList [((x, y), 1) | x <- [x0 .. x1], y <- [y0 .. y1]]
changeLightsStatusMap lights (TurnOff (x0, y0) (x1, y1)) =
  controlBrightness $ mapUnionWithCount lights newLights
  where
    newLights = Map.fromList [((x, y), -1) | x <- [x0 .. x1], y <- [y0 .. y1]]
changeLightsStatusMap lights (Toggle (x0, y0) (x1, y1)) =
  mapUnionWithCount lights newLights
  where
    newLights = Map.fromList [((x, y), 2) | x <- [x0 .. x1], y <- [y0 .. y1]]

applyCommandsSet :: LightsSet -> [Command] -> LightsSet
applyCommandsSet = List.foldl' changeLightsStatusSet

applyCommandsMap :: LightsMap -> [Command] -> LightsMap
applyCommandsMap = List.foldl' changeLightsStatusMap

mainSet :: IO ()
mainSet = do
  content <- readFile "input.txt"
  let linesList = lines content
  let commands = map (extractCommand . parseCommand) linesList
  let lightsLit = applyCommandsSet Set.empty commands

  print $ length lightsLit

mainMap :: IO ()
mainMap = do
  content <- readFile "input.txt"
  let linesList = lines content
  let commands = map (extractCommand . parseCommand) linesList
  let lightsLit = applyCommandsMap Map.empty commands

  print $ countBrightness lightsLit

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--part-1"] -> mainSet
    ["--part-2"] -> mainMap
    _ -> putStrLn "Usage: program_name (--part-1|--part-2)"
