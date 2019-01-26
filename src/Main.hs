module Main where

import System.IO    (readFile)
import Data.List    (lines, replicate)
import Data.Word
import Data.Char    (intToDigit)
import Data.Functor ((<$>))
import Data.List    (elem)
-- MODELS/TYPES

type List a = [a]
type DataRegisterAddress = Int
type State = Int

data RegisterMachine = RegisterMachine
    { -- Holds the address of the next instruction; starts at 1
      instructionRegister :: Word8
    , statusRegister      :: Word8
    , accumulator         :: Word8
    , dataRegisters       :: List Word8
    } deriving (Show)

-- AUTOMATON

checkInput :: State -> String -> State
checkInput s [] = s
checkInput s (x:xs) = case s of
    0  -> case x of
            'A' -> checkInput 1  xs
            'S' -> checkInput 4  xs
            'L' -> checkInput 11 xs
            'D' -> checkInput 15 xs
            'M' -> checkInput 20 xs
            'J' -> checkInput 24 xs
            'E' -> checkInput 34 xs
            _   -> checkInput 41 xs
    1  -> case x of
            'D' -> checkInput 2  xs
            _   -> checkInput 41 xs
    2  -> case x of
            'D' -> checkInput 3  xs
            _  -> checkInput 41 xs
    3  -> case x of
            ' ' -> checkInput 37 xs
            _   -> checkInput 41 xs
    4  -> case x of
            'U' -> checkInput 5  xs
            'T' -> checkInput 7  xs
            _   -> checkInput 41 xs
    5  -> case x of
            'B' -> checkInput 6  xs
            _   -> checkInput 41 xs
    6  -> case x of
            ' ' -> checkInput 37 xs
            _   -> checkInput 41 xs
    7  -> case x of
            'O' -> checkInput 8  xs
            _   -> checkInput 41 xs
    8  -> case x of
            'R' -> checkInput 9  xs
            _   -> checkInput 41 xs
    9  -> case x of
            'E' -> checkInput 10 xs
            _   -> checkInput 41 xs
    10 -> case x of
            ' ' -> checkInput 37 xs
            _   -> checkInput 41 xs
    11 -> case x of
            'O' -> checkInput 12 xs
            _   -> checkInput 41 xs
    12 -> case x of
            'A' -> checkInput 13 xs
            _   -> checkInput 41 xs
    13 -> case x of
            'D' -> checkInput 14 xs
            _   -> checkInput 41 xs
    14 -> case x of
            ' ' -> checkInput 37 xs
            _   -> checkInput 41 xs
    15 -> case x of
            'L' -> checkInput 16 xs
            _   -> checkInput 41 xs
    16 -> case x of
            'O' -> checkInput 17 xs
            _   -> checkInput 41 xs
    17 -> case x of
            'A' -> checkInput 18 xs
            _   -> checkInput 41 xs
    18 -> case x of
            'D' -> checkInput 19 xs
            _   -> checkInput 41 xs
    19 -> case x of
            ' ' -> checkInput 37 xs
            _   -> checkInput 41 xs
    20 -> case x of
            'U' -> checkInput 21 xs
            _   -> checkInput 41 xs
    21 -> case x of
            'L' -> checkInput 22 xs
            _   -> checkInput 41 xs
    22 -> case x of
            'T' -> checkInput 23 xs
            _   -> checkInput 41 xs
    23 -> case x of
            ' ' -> checkInput 37 xs
            _   -> checkInput 41 xs
    24 -> case x of
            'L' -> checkInput 25 xs
            'G' -> checkInput 28 xs
            'E' -> checkInput 30 xs
            'N' -> checkInput 32 xs
            _   -> checkInput 41 xs
    25 -> case x of
            'E' -> checkInput 26 xs
            _   -> checkInput 41 xs
    26 -> case x of
            ' ' -> checkInput 37 xs
            _   -> checkInput 41 xs
    27 -> case x of
            'G' -> checkInput 28 xs
            _   -> checkInput 41 xs
    28 -> case x of
            'T' -> checkInput 29 xs
            _   -> checkInput 41 xs
    29 -> case x of
            ' ' -> checkInput 37 xs
            _   -> checkInput 41 xs
    30 -> case x of
            'G' -> checkInput 31 xs
            _   -> checkInput 41 xs
    31 -> case x of
            ' ' -> checkInput 37 xs
            _   -> checkInput 41 xs
    32 -> case x of
            'E' -> checkInput 33 xs
            _   -> checkInput 41 xs
    33 -> case x of
            ' ' -> checkInput 37 xs
            _   -> checkInput 41 xs
    34 -> case x of
            'N' -> checkInput 35 xs
            _   -> checkInput 41 xs
    35 -> case x of
            'D' -> checkInput 36 xs
            _   -> checkInput 41 xs
    36 -> checkInput 41 xs
    37 -> case x of
            x1
               | x1 == '0'                         -> checkInput 38 xs
               | x1 `elem` (intToDigit <$> [1..9]) -> checkInput 39 xs
               | otherwise                         -> checkInput 41 xs
    38 -> case x of
            '\n' -> checkInput 0  xs
            _    -> checkInput 41 xs
    39 -> case x of
            x1
               | x1 == '0'                         -> checkInput 40 xs
               | x1 `elem` (intToDigit <$> [1..9]) -> checkInput 39 xs
               | x1 == '\n'                        -> checkInput 0  xs
               | otherwise                         -> checkInput 41 xs
    40 -> case x of
            x1
               | x1 == '0'                         -> checkInput 40 xs
               | x1 `elem` (intToDigit <$> [1..9]) -> checkInput 39 xs
               | x1 == '\n'                        -> checkInput 0  xs
               | otherwise                         -> checkInput 41 xs

-- HELPERS

-- RegisterMachine with n data registers
newRegisterMachine :: Int -> RegisterMachine
newRegisterMachine n = RegisterMachine 0 0 0 (replicate n 0)

-- Replace an element of a list with another one at a given index
-- DOES NOT assume the index starts at 1!
replaceNthElem :: (Num a, Ord a) => a -> Int -> [a] -> [a]
replaceNthElem elem i list
    | elem < 0 || (length list) <= i =
        list
    | otherwise =
        (take i list) ++ [elem] ++ (drop (i+1) list)

-- Moves onto the address of the next instruction
nextInstruction :: Word8 -> Word8
nextInstruction n = n + 1

-- Takes a RegisterMachine and an instruction
applyToRegisterMachine :: RegisterMachine
                       -> (RegisterMachine -> RegisterMachine)
                       -> RegisterMachine
applyToRegisterMachine reg f = f reg

-- INSTRUCTIONS

-- Takes a number, returns a function that can be applied on RegisterMachine.
dload :: Int -> (RegisterMachine -> RegisterMachine)
dload n = \(RegisterMachine instr stat acc dat) ->
    RegisterMachine (nextInstruction instr) stat n8 dat
  where n8 = fromIntegral n :: Word8

-- Takes an address, returns a function that can be applied on RegisterMachine.
store :: DataRegisterAddress -> (RegisterMachine -> RegisterMachine)
store addr = \(RegisterMachine instr stat acc dat) ->
    RegisterMachine
      (nextInstruction instr) stat acc (replaceNthElem acc (addr - 1) dat)

main :: IO ()
main = do
    putStrLn "Done"
