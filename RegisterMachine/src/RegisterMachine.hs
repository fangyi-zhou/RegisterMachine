module RegisterMachine where

import Data.List (intercalate)
import Control.Monad.State
import Data.Vector (Vector, (//), (!), fromList)
import qualified Data.Vector (length)

data Instruction = Add Int Int | Sub Int Int Int | Halt

data BracketedExpr = SingleAngBracket (Integer, Integer)
                   | DoubleAngBracket (Integer, Integer)

newtype RegMachine = RegMachine [Instruction]

type RegMachineExecutor = State (Vector Instruction)

executeInstructions :: Vector Integer -> Int -> RegMachineExecutor [(Int, Vector Integer)]
executeInstructions regs currInstr = do
  instrs <- get
  if Data.Vector.length instrs <= currInstr
  then fail "Label exceeded Instruction Length"
  else
    case instrs ! currInstr of
      Halt -> return []
      Add reg nextLabel ->
        if fromIntegral (length regs) <= reg
        then fail "Not Enough Registers"
        else do
          let updated = regs // [(reg, (regs ! reg) + 1)]
          next <- executeInstructions updated nextLabel
          return ((nextLabel, updated) : next)
      Sub reg succLabel failLabel
        | length regs <= reg ->
            fail "Not Enough Registers"
        | regs ! reg == 0 -> do
            next <- executeInstructions regs failLabel
            return ((failLabel, regs) : next)
        | otherwise -> do
            let updated = regs // [(reg, regs ! reg - 1)]
            next <- executeInstructions updated succLabel
            return ((succLabel, updated) : next)

runRegisterMachine :: RegMachine -> Int -> [Integer] -> [(Int, Vector Integer)]
runRegisterMachine (RegMachine instrs) startLabel config
  = (startLabel, fromList config) : evalState (executeInstructions (fromList config) startLabel) (fromList instrs)

class Serialisable a where
  serialise :: a -> Integer

instance Serialisable BracketedExpr where
  serialise (SingleAngBracket (x, y))
    = (2 ^ x) * (2 * y + 1) - 1
  serialise (DoubleAngBracket (x, y))
    = (2 ^ x) * (2 * y + 1)

instance Show BracketedExpr where
  show (SingleAngBracket (x, y))
    = "<" ++ show x ++ ", " ++ show y ++ ">"
  show (DoubleAngBracket (x, y))
    = "<<" ++ show x ++ ", " ++ show y ++ ">>"

instance Serialisable Instruction where
  serialise (Add reg label)
    = serialise (DoubleAngBracket (fromIntegral $ 2 * reg, fromIntegral label))
  serialise (Sub reg label1 label2)
    = serialise (DoubleAngBracket (
        fromIntegral $ 2 * reg + 1,
        serialise (SingleAngBracket (fromIntegral label1, fromIntegral label2))
        ))
  serialise Halt
    = 0

instance Show Instruction where
  show (Add reg label)
    = "R" ++ show reg ++ "+ -> L" ++ show label
  show (Sub reg label1 label2)
    = "R" ++ show reg ++ "- -> L" ++ show label1 ++ ", L" ++ show label2
  show Halt
    = "Halt"

instance Show RegMachine where
  show (RegMachine instrs)
    = intercalate " \n" (zipWith appendLabel instrs [0..])
    where
      appendLabel inst label
        = "L" ++ show label ++ ": " ++ show inst

instance Serialisable RegMachine where
  serialise (RegMachine [])
    = 0
  serialise (RegMachine (x : xs))
    = serialise (DoubleAngBracket (serialise x, serialise (RegMachine xs)))

decodeSingleBracketExpr :: Integer -> BracketedExpr
decodeSingleBracketExpr val
  = SingleAngBracket (decodeBracketExprHelper (val + 1))

decodeDoubleBracketExpr :: Integer -> BracketedExpr
decodeDoubleBracketExpr val
  | val == 0  = error "Illegal Serialisation"
  | otherwise = DoubleAngBracket (decodeBracketExprHelper val)

decodeInstruction :: Integer -> Instruction
decodeInstruction 0 = Halt
decodeInstruction val
  | even x    = Add (div x 2) y
  | otherwise = Sub (div (x - 1) 2) j k
  where
    DoubleAngBracket (x', y') = decodeDoubleBracketExpr val
    SingleAngBracket (j', k') = decodeSingleBracketExpr y'
    x = fromIntegral x'
    y = fromIntegral y'
    j = fromIntegral j'
    k = fromIntegral k'

decodeList :: Integer -> [Integer]
decodeList 0   = []
decodeList val = x : decodeList y
  where
    DoubleAngBracket (x, y) = decodeDoubleBracketExpr val

decodeRegMachine :: Integer -> RegMachine
decodeRegMachine val
  = RegMachine (map decodeInstruction (decodeList val))

decodeBracketExprHelper :: Integer -> (Integer, Integer)
decodeBracketExprHelper val
  | odd val   = (0, div (val - 1) 2)
  | otherwise = (1 + pow', rem')
  where
    (pow', rem') = decodeBracketExprHelper (div val 2)
