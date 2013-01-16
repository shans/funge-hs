module FungeBase where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader

-- maximum accepted stack value for nasty instructions like Iterate and BeginBlock/EndBlock
instruction_bound :: Integer
instruction_bound = 1024

type Location = [Int]
type Direction = [Int]
type Offset = [Int]
data TurnDirection = Left | Right deriving Show

data Instruction = Go Direction Char
                 | TurnLeft | TurnRight | GoAway
                 | AbsoluteVector
		 | Reverse Int
                 | Trampoline
                 | JumpOver | JumpForward
                 | Iterate
                 | StringMode | FetchCharacter | StoreCharacter
                 | LogicalNot | GreaterThan
                 | If Direction Direction Char | Compare
                 | NOp | Empty | OutOfBounds 
                 | Push Int Char | Pop | Duplicate | Swap | ClearStack
                 | Add | Multiply | Subtract | Divide | Remainder
                 | Stop | Quit
		 | BeginBlock | EndBlock | StackUnderStack
		 | Get | Put
		 | OutputDecimal | OutputCharacter
		 | InputDecimal | InputCharacter
                 | Print deriving Show

type Stack a = [a]

data InstructionValue = Cached Instruction | Gen (IO Instruction)
type InstructionMap = M.Map [Int] InstructionValue 
type Bounds = [(Int, Int)]
type InstructionPointer = (Location, Direction, Offset)

data VM = VM InstructionMap Bounds (Stack (Stack Int)) InstructionPointer Bool (Maybe Char)

data Config = Config { 
  charSet :: String, 
  trace :: VMRunner (), 
  printer :: [Int] -> String -> IO (),
  acceptUserInput :: Bool,
  activeExtensions :: String,
  randomizer :: [Int] -> VMRunner (), 
  displayOutput :: Bool
  }

type VMConfig = ReaderT Config IO

type VMRunner = StateT VM VMConfig

io :: IO a -> VMRunner a
io = lift . lift

config :: VMConfig a -> VMRunner a
config = lift

-- CONFIGURATION --

ipStackInstructionBoundsTrace = do
  ip <- getIP
  stack <- getStack
  instruction <- nextInstruction
  bounds <- getBounds
  io $ putStrLn (show ip ++ " : " ++ show instruction ++ " : " ++ show stack ++ " : " ++ show bounds)

noTrace :: VMRunner ()
noTrace = return ()

show2DFunge :: [Int] -> String -> IO ()
show2DFunge dim field = mapM_ putStrLn (splitEvery (last dim) field)

funge2DToString :: [Int] -> String -> String
funge2DToString dim field = unlines $ splitEvery (last dim) field

splitEvery :: Int -> [a] -> [[a]]
splitEvery n l | length l < n = [l]
splitEvery n l = (take n l):splitEvery n (drop n l)

noPrinter :: [Int] -> String -> IO ()
noPrinter _ _ = return ()

getCharSet :: VMConfig String
getCharSet = liftM charSet ask

getTrace :: VMConfig (VMRunner ())
getTrace = liftM trace ask

getPrinter :: VMConfig ([Int] -> String -> IO ())
getPrinter = liftM printer ask

getAcceptUserInput :: VMConfig Bool
getAcceptUserInput = liftM acceptUserInput ask

getActiveExtensions :: VMConfig String
getActiveExtensions = liftM activeExtensions ask

getRandomizer :: VMConfig ([Int] -> VMRunner ())
getRandomizer = liftM randomizer ask

getDisplayOutput :: VMConfig Bool
getDisplayOutput = liftM displayOutput ask

-- STATE --

getInstructionMap :: VMRunner InstructionMap
getInstructionMap = do
  VM im _ _ _ _ _ <- get
  return im

putInstructionMap :: InstructionMap -> VMRunner ()
putInstructionMap im = do
  VM _ a b c d e <- get
  put (VM im a b c d e)

getIP :: VMRunner InstructionPointer
getIP = do  
  VM _ _ _ ip _ _ <- get
  return ip

putIP :: InstructionPointer -> VMRunner ()
putIP ip = do
  VM a b c _ d e <- get
  put (VM a b c ip d e)

getStack :: VMRunner (Stack (Stack Int))
getStack = do
  VM _ _ stack _ _ _ <- get
  return stack

putStack :: (Stack (Stack Int)) -> VMRunner ()
putStack stack = do
  VM a b _ c d e  <- get
  put (VM a b stack c d e)

getBounds :: VMRunner Bounds
getBounds = do
  VM _ bounds _ _ _ _ <- get
  return bounds

putBounds :: Bounds -> VMRunner ()
putBounds bounds = do
  VM a _ b c d e <- get
  put (VM a bounds b c d e)

setStringMode :: VMRunner ()
setStringMode = do
  VM a b c d _ e <- get
  put (VM a b c d True e)

clearStringMode :: VMRunner ()
clearStringMode = do
  VM a b c d _ e <- get
  put (VM a b c d False e)

isInStringMode :: VMRunner Bool
isInStringMode = do
  VM _ _ _ _ sm _ <- get
  return sm

getFirstChar :: VMRunner (Maybe Char)
getFirstChar = do
  VM a b c d e f <- get
  put (VM a b c d e Nothing)
  return f

pushBack :: Char -> VMRunner ()
pushBack char = do
  VM a b c d e Nothing <- get
  put (VM a b c d e (Just char))

instructionAt :: Location -> VMRunner Instruction
instructionAt loc = do
  map <- getInstructionMap
  case M.lookup loc map of
    Just (Cached a)  -> return a
    Just (Gen a) -> do
      i <- io a
      putInstructionMap $ M.insert loc (Cached i) map
      return i
    Nothing -> return Empty

nextInstruction :: VMRunner Instruction
nextInstruction = do
  bounds <- getBounds
  (loc, _, _) <- getIP
  if loc `outside` bounds
  then return OutOfBounds
  else instructionAt loc 

outside :: Location -> Bounds -> Bool
outside loc bounds = or $ zipWith isOut loc bounds
  where
    isOut x (min, max) = x < min || x >= max
