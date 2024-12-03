module Main where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.Map as Map
import Data.Word (Word16)
import Parser (Op (..), Operand (..), parseOp)
import System.Environment (getArgs)
import Text.Parsec (ParseError)

type Variables = Map.Map String Word16

extractOp :: Either ParseError Op -> Op
extractOp command = case command of
  Left err -> error $ "Parsing error: " ++ show err
  Right comm -> comm

evaluateOperand :: Variables -> Operand -> Maybe Word16
evaluateOperand _ (Number n) = Just n
evaluateOperand vars (Id var) = Map.lookup var vars

runOp :: Variables -> Op -> Maybe Variables
runOp vars op = case op of
  AND op1 op2 attr ->
    binaryOp (.&.) vars op1 op2 attr
  OR op1 op2 attr ->
    binaryOp (.|.) vars op1 op2 attr
  LSHIFT op1 op2 attr ->
    binaryOp (\x y -> x `shiftL` fromIntegral y) vars op1 op2 attr
  RSHIFT op1 op2 attr ->
    binaryOp (\x y -> x `shiftR` fromIntegral y) vars op1 op2 attr
  ATTR operator attr ->
    unaryOp id vars operator attr
  NOT operator attr ->
    unaryOp complement vars operator attr

binaryOp :: (Word16 -> Word16 -> Word16) -> Variables -> Operand -> Operand -> String -> Maybe Variables
binaryOp f vars op1 op2 attr = do
  val1 <- evaluateOperand vars op1
  val2 <- evaluateOperand vars op2
  let result = f val1 val2
  return $ Map.insert attr result vars

unaryOp :: (Word16 -> Word16) -> Variables -> Operand -> String -> Maybe Variables
unaryOp f vars operator attr = do
  val <- evaluateOperand vars operator
  let result = f val
  return $ Map.insert attr result vars

processOps :: Variables -> [Op] -> Variables
processOps vars [] = vars
processOps vars ops =
  let (newVars, remainingOps) = foldl evalOp (vars, []) ops
   in if null remainingOps
        then newVars -- All done
        else processOps newVars remainingOps -- Process unresolved operations

evalOp :: (Variables, [Op]) -> Op -> (Variables, [Op])
evalOp (vars, pending) op = case runOp vars op of
  Just updatedVars -> (updatedVars, pending) -- Successfully evaluated
  Nothing -> (vars, op : pending) -- Deferred for later

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Wrong number of arguments."
    (star : file : _) -> do
      case star of
        "1" -> main1 file
        "2" -> main2 file
        _ -> putStrLn "Invalid star number given"
    _ -> putStrLn "Wrong number of arguments."

evaluateA :: Variables -> Word16
evaluateA vars =
  case Map.lookup "a" vars of
    Just value -> value
    Nothing -> error "Could not find value for signal a"

main1 :: String -> IO ()
main1 file = do
  content <- readFile file
  let linesInput = lines content
  let ops = map (extractOp . parseOp) linesInput
  let finalVars = processOps Map.empty ops
  print finalVars
  print $ evaluateA finalVars

main2 :: String -> IO ()
main2 file = do
  content <- readFile file
  let linesInput = lines content
      ops = map (extractOp . parseOp) linesInput
      finalVars = processOps Map.empty ops
      a = evaluateA finalVars
      filteredOps =
        filter
          ( \op -> case op of
              ATTR _ "b" -> False
              _ -> True
          )
          ops
      newOps = filteredOps ++ [ATTR (Number a) "b"]
      newFinalVars = processOps Map.empty newOps

  print newFinalVars
  print $ evaluateA newFinalVars
