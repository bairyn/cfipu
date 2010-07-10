module Data.Memory ( Memory
                   , Cell
                   , initialMemory
                   , word8sToCells
                   , stringToCells
                   , charToCell
                   , word8ToCell
                   , defaultCell
                   , cellToWord8
                   , cellToChar
                   , cellsToWord8s
                   , cellsToString
                   , moveInstructionPointerLeft
                   , moveInstructionPointerRight
                   , moveDataPointerLeft
                   , moveDataPointerRight
                   , moveInstructionPointerToDataPointer
                   , moveDataPointerToInstructionPointer
                   , getInstruction
                   , getData
                   , setData
                   , modifyData
                   ) where

import Prelude hiding (null)
import Data.ByteString.Internal (c2w, w2c)
import Data.Dequeue
import Data.Maybe (fromJust)
import Data.Word (Word8)

type Cell              = Word8
type InternalContainer = BankersDequeue Cell  -- Back is right end; Front is left end

data Memory = ID { id_p :: InternalContainer
                 , id_i :: InternalContainer
                 , id_d :: InternalContainer
                 }
            | DI { di_p :: InternalContainer
                 , di_d :: InternalContainer
                 , di_i :: InternalContainer
                 }
            | E  { e_p  :: InternalContainer
                 , e_e  :: InternalContainer
                 }

initialMemory :: [Cell] -> Memory
initialMemory = E empty . foldr (flip pushFront) empty

defaultCell :: Cell
defaultCell = 0x00

word8ToCell :: Word8 -> Cell
word8ToCell = id

charToCell :: Char -> Cell
charToCell = c2w

word8sToCells :: [Word8] -> [Cell]
word8sToCells = map word8ToCell

stringToCells :: String -> [Cell]
stringToCells = map charToCell

cellToWord8 :: Cell -> Word8
cellToWord8 = id

cellToChar :: Cell -> Char
cellToChar = w2c

cellsToWord8s :: [Cell] -> [Word8]
cellsToWord8s = map cellToWord8

cellsToString :: [Cell] -> String
cellsToString = map cellToChar

moveInstructionPointerLeft :: Memory -> Memory
moveInstructionPointerLeft (ID p i d) =
    case popBack p of
        ((Just cell), p') -> ID p' (pushFront i cell)        d
        ((Nothing),   _)  -> ID p  (pushFront i defaultCell) d
moveInstructionPointerLeft (DI p d i) =
    case popBack d of
        ((Just cell), d') -> case () of _
                                           | null d'   -> E  p    $ pushFront i cell
                                           | otherwise -> DI p d' $ pushFront i cell
        ((Nothing),   _)  -> moveInstructionPointerLeft $ E p i
moveInstructionPointerLeft (E  p e) =
    case popBack p of
        ((Just cell), p') -> ID p' (pushFront empty cell)        e
        ((Nothing),   _)  -> ID p  (pushFront empty defaultCell) e

moveInstructionPointerRight :: Memory -> Memory
moveInstructionPointerRight (ID p i d) =
    case popFront i of
        ((Just cell), i') -> case () of _
                                           | null i'   -> E  (pushBack p cell) d
                                           | otherwise -> ID (pushBack p cell) i' d
        ((Nothing),   _)  -> moveInstructionPointerRight $ E p i
moveInstructionPointerRight (DI p d i) =
    case popFront i of
        ((Just cell), i') -> case () of _
                                           | null i'   -> DI p (pushBack d cell) $ pushBack i' defaultCell
                                           | otherwise -> DI p (pushBack d cell) $ i'
        ((Nothing),   _)  -> moveInstructionPointerRight $ E p d
moveInstructionPointerRight (E  p e) =
    case popFront e of
        ((Just cell), e') -> case () of _
                                           | null e'   -> DI p (pushBack empty cell) $ pushBack empty defaultCell
                                           | otherwise -> DI p (pushBack empty cell) e'
        ((Nothing),   _)  -> moveInstructionPointerRight $ E p $ pushBack empty defaultCell

moveDataPointerLeft :: Memory -> Memory
moveDataPointerLeft (ID p i d) =
    case popBack i of
        ((Just cell), i') -> case () of _
                                           | null i'   -> E  p    $ pushFront d cell
                                           | otherwise -> ID p i' $ pushFront d cell
        ((Nothing),   _)  -> moveDataPointerLeft $ E p d
moveDataPointerLeft (DI p d i) =
    case popBack p of
        ((Just cell), p') -> DI p' (pushFront d cell)        i
        ((Nothing),   _)  -> DI p  (pushFront d defaultCell) i
moveDataPointerLeft (E  p e) =
    case popBack p of
        ((Just cell), p') -> DI p' (pushFront empty cell)        e
        ((Nothing),   _)  -> DI p  (pushFront empty defaultCell) e

moveDataPointerRight :: Memory -> Memory
moveDataPointerRight (ID p i d) =
    case popFront d of
        ((Just cell), d') -> case () of _
                                           | null d'   -> ID p (pushBack i cell) $ pushBack d' defaultCell
                                           | otherwise -> ID p (pushBack i cell) $ d'
        ((Nothing),   _)  -> moveDataPointerRight $ E p i
moveDataPointerRight (DI p d i) =
    case popFront d of
        ((Just cell), d') -> case () of _
                                           | null d'   -> E  (pushBack p cell) i
                                           | otherwise -> DI (pushBack p cell) d' i
        ((Nothing),   _)  -> moveDataPointerRight $ E p d
moveDataPointerRight (E  p e) =
    case popFront e of
        ((Just cell), e') -> case () of _
                                           | null e'   -> ID p (pushBack empty cell) $ pushBack empty defaultCell
                                           | otherwise -> ID p (pushBack empty cell) e'
        ((Nothing),   _)  -> moveDataPointerRight $ E p $ pushBack empty defaultCell

moveInstructionPointerToDataPointer :: Memory -> Memory
moveInstructionPointerToDataPointer mem@(ID {}) = moveInstructionPointerToDataPointer . moveInstructionPointerRight $ mem
moveInstructionPointerToDataPointer mem@(DI {}) = moveInstructionPointerToDataPointer . moveInstructionPointerLeft  $ mem
moveInstructionPointerToDataPointer mem@(E  {}) = mem

moveDataPointerToInstructionPointer :: Memory -> Memory
moveDataPointerToInstructionPointer mem@(ID {}) = moveDataPointerToInstructionPointer . moveDataPointerLeft  $ mem
moveDataPointerToInstructionPointer mem@(DI {}) = moveDataPointerToInstructionPointer . moveDataPointerRight $ mem
moveDataPointerToInstructionPointer mem@(E  {}) = mem

getInstruction :: Memory -> Cell
getInstruction (ID p i d) =
    case popFront i of
        ((Just cell), _) -> cell
        ((Nothing),   _) -> getInstruction $ E p i
getInstruction (DI p d i) =
    case popFront i of
        ((Just cell), _) -> cell
        ((Nothing),   _) -> defaultCell
getInstruction (E  p e) =
    case popFront e of
        ((Just cell), _) -> cell
        ((Nothing),   _) -> defaultCell

getData :: Memory -> Cell
getData (ID p i d) =
    case popFront d of
        ((Just cell), _) -> cell
        ((Nothing),   _) -> defaultCell
getData (DI p d i) =
    case popFront d of
        ((Just cell), _) -> cell
        ((Nothing),   _) -> getData $ E p i
getData (E  p e) =
    case popFront e of
        ((Just cell), _) -> cell
        ((Nothing),   _) -> defaultCell

setData :: Cell -> Memory -> Memory
setData cell mem@(ID p i d) = mem{id_d = flip pushFront cell . snd $ popFront d}
setData cell mem@(DI p d i)
    | null d    = setData cell $ E p i
    | otherwise = mem{di_d  = flip pushFront cell . snd $ popFront d}
setData cell mem@(E  p e)   = mem{e_e  = flip pushFront cell . snd $ popFront e}

modifyData :: (Cell -> Cell) -> Memory -> Memory
modifyData f mem = flip setData mem . f . getData $ mem
