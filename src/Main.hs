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

checkInput :: String -> State -> State
checkInput [] s = s
checkInput (x:xs) s = case s of
    0  -> case x of
            'A' -> checkInput xs 1
            'S' -> checkInput xs 4
            'L' -> checkInput xs 11
            'D' -> checkInput xs 15
            'M' -> checkInput xs 20
            'J' -> checkInput xs 24
            'E' -> checkInput xs 34
            _   -> checkInput xs 41
    1  -> case x of
            'D' -> checkInput xs 2
            _   -> checkInput xs 41
    2  -> case x of
            'D' -> checkInput xs 3
            _   -> checkInput xs 41
    3  -> case x of
            ' ' -> checkInput xs 37
            _   -> checkInput xs 41
    4  -> case x of
            'U' -> checkInput xs 5
            'T' -> checkInput xs 7
            _   -> checkInput xs 41
    5  -> case x of
            'B' -> checkInput xs 6
            _   -> checkInput xs 41
    6  -> case x of
            ' ' -> checkInput xs 37
            _   -> checkInput xs 41
    7  -> case x of
            'O' -> checkInput xs 8
            _   -> checkInput xs 41
    8  -> case x of
            'R' -> checkInput xs 9
            _   -> checkInput xs 41
    9  -> case x of
            'E' -> checkInput xs 10
            _   -> checkInput xs 41
    10 -> case x of
            ' ' -> checkInput xs 37
            _   -> checkInput xs 41
    11 -> case x of
            'O' -> checkInput xs 12
            _   -> checkInput xs 41
    12 -> case x of
            'A' -> checkInput xs 13
            _   -> checkInput xs 41
    13 -> case x of
            'D' -> checkInput xs 14
            _   -> checkInput xs 41
    14 -> case x of
            ' ' -> checkInput xs 37
            _   -> checkInput xs 41
    15 -> case x of
            'L' -> checkInput xs 16
            _   -> checkInput xs 41
    16 -> case x of
            'O' -> checkInput xs 17
            _   -> checkInput xs 41
    17 -> case x of
            'A' -> checkInput xs 18
            _   -> checkInput xs 41
    18 -> case x of
            'D' -> checkInput xs 19
            _   -> checkInput xs 41
    19 -> case x of
            ' ' -> checkInput xs 37
            _   -> checkInput xs 41
    20 -> case x of
            'U' -> checkInput xs 21
            _   -> checkInput xs 41
    21 -> case x of
            'L' -> checkInput xs 22
            _   -> checkInput xs 41
    22 -> case x of
            'T' -> checkInput xs 23
            _   -> checkInput xs 41
    23 -> case x of
            ' ' -> checkInput xs 37
            _   -> checkInput xs 41
    24 -> case x of
            'L' -> checkInput xs 25
            'G' -> checkInput xs 28
            'E' -> checkInput xs 30
            'N' -> checkInput xs 32
            _   -> checkInput xs 41
    25 -> case x of
            'E' -> checkInput xs 26
            _   -> checkInput xs 41
    26 -> case x of
            ' ' -> checkInput xs 37
            _   -> checkInput xs 41
    27 -> case x of
            'G' -> checkInput xs 28
            _   -> checkInput xs 41
    28 -> case x of
            'T' -> checkInput xs 29
            _   -> checkInput xs 41
    29 -> case x of
            ' ' -> checkInput xs 37
            _   -> checkInput xs 41
    30 -> case x of
            'G' -> checkInput xs 31
            _   -> checkInput xs 41
    31 -> case x of
            ' ' -> checkInput xs 37
            _   -> checkInput xs 41
    32 -> case x of
            'E' -> checkInput xs 33
            _   -> checkInput xs 41
    33 -> case x of
            ' ' -> checkInput xs 37
            _   -> checkInput xs 41
    34 -> case x of
            'N' -> checkInput xs 35
            _   -> checkInput xs 41
    35 -> case x of
            'D' -> checkInput xs 36
            _   -> checkInput xs 41
    36 -> checkInput xs 41
    37 -> case x of
            x1
               | x1 == '0'                         -> checkInput xs 38
               | x1 `elem` (intToDigit <$> [1..9]) -> checkInput xs 39
               | otherwise                         -> checkInput xs 41
    38 -> case x of
            '\n' -> checkInput xs 0
            _    -> checkInput xs 41
    39 -> case x of
            x1
               | x1 == '0'                         -> checkInput xs 40
               | x1 `elem` (intToDigit <$> [1..9]) -> checkInput xs 39
               | x1 == '\n'                        -> checkInput xs 0
               | otherwise                         -> checkInput xs 41
    40 -> case x of
            x1
               | x1 == '0'                         -> checkInput xs 40
               | x1 `elem` (intToDigit <$> [1..9]) -> checkInput xs 39
               | x1 == '\n'                        -> checkInput xs 0
               | otherwise                         -> checkInput xs 41

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
