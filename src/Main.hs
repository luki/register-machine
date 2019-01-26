module Main where

import Automaton

import System.IO    (readFile)
import Data.List    (lines, replicate)
import Data.Word
import Data.List    (elem)

-- MODELS/TYPES

type List a = [a]
type DataRegisterAddress = Int

data RegisterMachine = RegisterMachine
    { -- Holds the address of the next instruction; starts at 1
      instructionRegister :: Word8
    , statusRegister      :: Word8
    , accumulator         :: Word8
    , dataRegisters       :: List Word8
    } deriving (Show)

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
    putStrLn "Test"
