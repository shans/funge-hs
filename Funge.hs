{-# LANGUAGE TypeSynonymInstances #-}

module Funge where

import FungeBase

import Data.Char
import qualified Data.Map as M
import Control.Monad.State
import System.Random

pop :: VMRunner Int
pop = do
  s <- getStack
  case s of 
    [] -> return 0
    (h:t) -> case h of
      [] -> return 0
      (h':t') -> do {putStack (t':t); return h'}

popn :: Int -> VMRunner [Int]
popn 0 = return []
popn n = do
  t <- popn (n - 1)
  h <- pop
  return (h:t)

push :: Int -> VMRunner ()
push n = do
  s <- getStack
  case s of 
    [] -> putStack [[n]]
    (h:t) -> putStack ((n:h):t)

pushn :: [Int] -> VMRunner ()
pushn [] = return ()
pushn (h:t) = do { push h; pushn t }

stackLength :: VMRunner Int
stackLength = do
  s <- getStack
  case s of
    [] -> return 0
    (h:t) -> return $ length h

popStack :: VMRunner Bool
popStack = do
  s <- getStack
  case s of
    [] -> return False
    [a] -> return False
    (h:t) -> do { putStack t; return True }

pushStack :: VMRunner ()
pushStack = do
  s <- getStack
  putStack $ []:s

stackUnderStack :: Int -> VMRunner ()
stackUnderStack count = do
  s <- getStack
  case s of
    [] -> execute' (Reverse (ord 'u'))
    [a] -> execute' (Reverse (ord 'u'))
    (h:h':t) -> if count >= 0 then sossToToss else tossToSoss where
      sossToToss = do
	let values = take count h'
	let values' = values ++ replicate (count - length values) 0
	putStack ((reverse values' ++ h):(drop count h'):t)
      tossToSoss = do
	let values = take (-count) h
	let values' = values ++ replicate ((-count) - length values) 0
	putStack ((drop (-count) h):(reverse values' ++ h'):t)
    
-----------

step :: VMRunner (Maybe Int) 
step = do
  sm <- isInStringMode
  if (not sm) then stepExecute else do { stepStringMode ; return Nothing }

stepExecute :: VMRunner (Maybe Int)
stepExecute = do
  stop <- checkInstruction
  case stop of
    Nothing -> do
      instruction <- nextInstruction
      stop <- execute instruction
      case stop of
        Nothing -> do
          updateIP
          ip <- getIP
          checkInstruction
        Just a -> return (Just a)
    Just a -> return (Just a)

stepStringMode :: VMRunner ()
stepStringMode = do
  instruction <- nextInstruction
  case instruction of
    OutOfBounds -> do {linearWrapIP; stepStringMode}
    Empty       -> do {push (ord ' '); updateIP; movePastEmpty}
    StringMode  -> do {clearStringMode; updateIP}
    Reverse n   -> do {push n; updateIP}
    _           -> do {push . ord $ toChar instruction; updateIP}

movePastEmpty = do
  instruction <- nextInstruction
  case instruction of
    OutOfBounds -> do {linearWrapIP; movePastEmpty}
    Empty       -> do {updateIP; movePastEmpty}
    _           -> return ()

checkInstruction = do
  instruction <- nextInstruction
  case instruction of
    Empty       -> do {updateIP; checkInstruction}
    OutOfBounds -> do {linearWrapIP; checkInstruction}
    Stop        -> return (Just 0)
    Quit        -> do {x <- pop; return (Just x)}
    _           -> return Nothing

jumpOver = do
  instruction <- nextInstruction
  case instruction of
    JumpOver    -> return Nothing
    OutOfBounds -> do {linearWrapIP; jumpOver}
    _           -> do {updateIP; jumpOver}

updateIP :: VMRunner ()
updateIP = do
  (loc, dir, offset) <- getIP
  putIP (update loc dir, dir, offset)

linearWrapIP :: VMRunner ()
linearWrapIP = do
  (loc, dir, offset) <- getIP
  bounds <- getBounds
  if leftOf loc dir bounds then do
    let loc' = updateUntilInside bounds loc dir
    putIP (loc', dir, offset)
  else do
    let dir' = map (*(-1)) dir
    let loc' = updateUntil bounds (updateUntilInside bounds loc dir') dir'
    putIP (loc', dir, offset)
      where
        updateUntilInside bounds loc dir = if outside loc bounds then updateUntilInside bounds (update loc dir) dir else loc
        updateUntil bounds loc dir = if outside loc' bounds then loc else updateUntil bounds loc' dir
          where
            loc' = update loc dir

leftOf [] [] [] = False
leftOf (h:t) (h':t') ((l,r):t'') = if (h' > 0 && h < l) || (h' < 0 && h > l) then True else leftOf t t' t''

execute :: Instruction -> VMRunner (Maybe Int)
execute Stop = return (Just 0)
execute Quit = do {x <- pop; return (Just x)}
execute JumpOver = do {updateIP; jumpOver}
-- wow, this is tricky. The spec is .. underspecced (http://quadium.net/funge/spec98.html)
-- There's a tutorial with strongly specified behaviour here: http://www.rcfunge98.com/tutorial4.html
-- but the behaviour there .. uh, it's batshit fucking loco.
-- the important questions are:
-- (1) should iterate execute' in the iterate cell or in the iterated instruction's cell?
-- (2) should the next instruction execute'd after iterate in general be the iterated instruction again?
--     (i.e. will 3k+ execute' + 4 times not 3?)
-- (3) should there be exceptional behaviour if '0' is the top value on the stack?
-- the tutorial's position is (1): current cell, (2): yes, (3): yes - 0k+ should exceute + 0 times, not 1
-- I'm going to go with (1): current cell, (2): yes, (3): no - all iterates should execute' the iterated instruction.
execute Iterate = do
  (loc, dir, offset) <- getIP
  count <- pop
  if (count > instruction_bound) then do { push count; execute' $ Reverse (ord 'k'); return Nothing } else do
    updateIP
    checkInstruction
    instruction <- nextInstruction
    putIP (loc, dir, offset)
    if (count > 0) then do
      forM_ [1..(count-1)] (\a -> execute instruction)
      execute instruction
    else return Nothing
execute n = execute' n >> return Nothing

execute' :: Instruction -> VMRunner ()
execute' (Go dir _) = do
  (loc, _, offset) <- getIP
  putIP (loc, dir, offset)
execute' (Reverse _) = do
  (loc, dir, offset) <- getIP
  putIP (loc, map (*(-1)) dir, offset)
execute' TurnLeft = do
  (loc, a:b:rest, offset) <- getIP
  putIP (loc, b:(-a):rest, offset)
execute' TurnRight = do
  (loc, a:b:rest, offset) <- getIP
  putIP (loc, (-b):a:rest, offset)
execute' AbsoluteVector = do
  (loc, dir, offset) <- getIP
  newDir <- popn (length dir)
  putIP (loc, newDir, offset)
execute' Trampoline = do
  (loc, dir, offset) <- getIP
  putIP (update loc dir, dir, offset)
execute' JumpForward = do
  (loc, dir, offset) <- getIP
  amount <- pop
  putIP (update loc (map (*amount) dir), dir, offset)
execute' (Push n _) = push n
execute' Add = do {a <- pop; b <- pop; push (a + b)}
execute' Multiply = do {a <- pop; b <- pop; push (a * b)}
execute' Subtract = do {a <- pop; b <- pop; push (b - a)}
execute' Divide = do {a <- pop; b <- pop; push $ if a == 0 then 0 else (b `div` a)}
execute' Remainder = do {a <- pop; b <- pop; push $ if a == 0 then 0 else (b `mod` a)}
execute' Pop = pop >> return ()
execute' Duplicate = do {a <- pop; push a; push a}
execute' Swap = do {a <- pop; b <- pop; push a; push b}
execute' ClearStack = clearStack
execute' NOp = return () 
execute' LogicalNot = do { a <- pop; push (if a == 0 then 1 else 0) }
execute' GreaterThan = do { a <- pop; b <- pop; push (if b > a then 1 else 0) }
execute' (If zeroDir nonZeroDir c) = do { a <- pop; if a == 0 then execute' (Go zeroDir c) else execute' (Go nonZeroDir c)}
execute' Compare = do { b <- pop; a <- pop; if (a < b) then execute' TurnLeft else if (a > b) then execute' TurnRight else execute' NOp } 
execute' StringMode = setStringMode
execute' Print = do { extensions <- config getActiveExtensions; if 'P' `elem` extensions then printStack else execute' (Reverse (ord 'P')) }
execute' FetchCharacter = do {execute' Trampoline; fetchCharacter}
execute' StoreCharacter = do
  char <- pop
  (loc, dir, offset) <- getIP
  let loc' = update loc dir
  im <- getInstructionMap
  putInstructionMap $ M.insert loc' (dataToCell (length loc) char) im
  bounds <- getBounds
  putBounds $ zipWith (\x (l,h) -> (min x l, max (x + 1) h)) loc' bounds
execute' GoAway = do
  r <- io randomIO
  (loc, dir, offset) <- getIP
  let n = r `mod` (length loc * 2)
  let pos = n `div` 2
  let dir' = replicate pos 0 ++ ((n `mod` 2) * 2 - 1):(replicate (length loc - pos - 1) 0)
  putIP (loc, dir', offset)
-- Modified to fail if the frame size is excessively large.
execute' BeginBlock = do
  n <- pop
  if (abs n > instruction_bound) then do { push n; execute' (Reverse (ord '{')); } else do 
    values <- if n >= 0 then popn n else return (replicate (-n) 0)
    (loc, dir, offset) <- getIP
    pushn offset
    putIP (loc, dir, update loc dir)
    pushStack
    pushn values
execute' EndBlock = do
  n <- pop
  if (abs n > instruction_bound) then do { push n; execute' (Reverse (ord '}')); } else do
    l <- stackLength
    values <- if n >= 0 then popn n else return []
    popped <- popStack
    if popped then do
      (loc, dir, _) <- getIP
      offset <- popn (length loc)
      putIP (loc, dir, offset)
      pushn values
      _ <- if n < 0 then popn (-n) else return []
      return ()
    else do
      pushn . take l $ values
      push n
      execute' (Reverse $ ord '}')
execute' StackUnderStack = do
  count <- pop
  stackUnderStack count
execute' Get = do
  (loc, dir, offset) <- getIP
  getLoc <- popn (length offset)
  instruction <- instructionAt $ update getLoc offset
  push . cellToData $ instruction 
execute' Put = do
  (loc, dir, offset) <- getIP
  putLoc <- popn (length offset)
  if (length (filter ((> instruction_bound) . abs) putLoc) > 0) then do { pushn putLoc; execute' (Reverse (ord 'p')) } else do
    instruction <- pop
    im <- getInstructionMap
    putInstructionMap $ M.insert (update putLoc offset) (dataToCell (length offset) instruction) im
    bounds <- getBounds
    putBounds $ zipWith (\x (l,h) -> (min x l, max (x + 1) h)) (update putLoc offset) bounds
execute' OutputDecimal = do
  num <- pop
  io $ putStr (show num ++ " ")
execute' OutputCharacter = do
  char <- pop
  if (char >= 0 && char <= 1114111) then io $ putStr (chr char:"") else execute' (Reverse char)
execute' InputDecimal = do
  doInput <- config getAcceptUserInput
  if doInput then inputDecimal else execute' (Reverse (ord '&'))
execute' InputCharacter = do
  doInput <- config getAcceptUserInput
  if doInput then do
    char <- getAChar
    push $ ord char
  else
    execute' (Reverse (ord '~'))

inputDecimal = do
  clearNonNumeric
  chars <- getNumeric
  push $ foldl (\t v -> t * 10 + v) 0 chars

clearNonNumeric = do
  char <- getAChar
  if isNumber char then pushBack char else clearNonNumeric 

getNumeric = do
  char <- getAChar
  if isNumber char
  then do
    a <- getNumeric
    return $ (ord char - ord '0'):a
  else do
    pushBack char
    return []

getAChar = do
  x <- getFirstChar
  case x of
    Nothing -> io $ getChar
    (Just a) -> return a

fetchCharacter = do
  instruction <- nextInstruction
  case instruction of
    OutOfBounds -> do {linearWrapIP; fetchCharacter}
    Empty       -> push (ord ' ')
    StringMode  -> push (ord '"')
    _           -> push . ord $ toChar instruction
    
printStack = do
  a <- pop
  case a of
    n | n > 0 && n <= 1114111 -> do { io $ putChar (chr n); printStack }
    _ -> return ()

clearStack = do
  a <- getStack
  case a of 
    [] -> return ()
    (h:t) -> do { putStack ([]:t) ; return () }

update :: Location -> Direction -> Location
update [] [] = []
update (a:b) (c:d) = (a+c):(update b d)

emptyMap :: InstructionMap
emptyMap = M.empty

input :: String -> [Int] -> [Int] -> VMRunner ()
input d pos size = do
  map <- getInstructionMap
  let map' = input' d map pos (reverse size)
  putInstructionMap map'
  bounds <- getBounds
  let myBounds = zipWith (\a b -> (a, a + b)) pos size
  let bounds' = if length bounds == 0 then myBounds else zipWith (\(a, b) (c, d) -> (min a c, max b d)) myBounds bounds
  putBounds bounds'
  where
    input' d m pos [n] = layoutRow d m pos n
    input' d m pos (0:t) = m
    input' d m pos (h:t) = input' restOfD (input' d m pos t) (incPos (length t) pos) (h-1:t)
      where
        restOfD = drop (product t) d
        incPos 0 (h:t) = (h+1:t)
        incPos n (h:t) = h:(incPos (n-1) t)

layoutRow :: String -> InstructionMap -> [Int] -> Int -> InstructionMap
layoutRow d m pos 0 = m
layoutRow (h:t) m pos@(ph:pt) n = layoutRow t (updateNonEmpty pos (decodeInstruction (length pos) h) m) (ph+1:pt) (n-1)
layoutRow d m pos n = error $ (show d) ++ " " ++ (show m) ++ " " ++ show pos ++ " " ++ show n

updateNonEmpty :: [Int] -> Instruction -> InstructionMap -> InstructionMap
updateNonEmpty _ Empty m = m
updateNonEmpty k v m = M.insert k v $ m

toChar Empty = ' '
toChar Trampoline = '#'
toChar JumpOver = ';'
toChar JumpForward = 'j'
toChar Stop = '@'
toChar Quit = 'q'
toChar Add ='+'
toChar Multiply = '*'
toChar Subtract = '-'
toChar Divide = '/'
toChar Remainder = '%'
toChar Pop = '$'
toChar Duplicate = ':'
toChar Swap = '\\'
toChar ClearStack = 'n'
toChar AbsoluteVector = 'x'
toChar Iterate = 'k'
toChar LogicalNot = '!' 
toChar GreaterThan = '`'
toChar StringMode = '"'
toChar FetchCharacter = '\''
toChar StoreCharacter = 's'
toChar GoAway = '?'
toChar (If _ _ c) = c
toChar Compare = 'w'
toChar TurnLeft = '['
toChar TurnRight = ']'
toChar (Push _ c) = c
toChar (Go _ c) = c
toChar Print = 'P'
toChar BeginBlock = '{'
toChar EndBlock = '}'
toChar StackUnderStack = 'u'
toChar Get = 'g'
toChar Put = 'p'
toChar OutputDecimal = '.'
toChar OutputCharacter = ','
toChar InputDecimal = '&'
toChar InputCharacter = '~'
toChar (Reverse c) | c >= 0 && c <= 1114111 = chr c
toChar i = error $ "no toChar mapping for " ++ show i

cellToData :: Instruction -> Int
cellToData (Reverse c) = c
cellToData x = ord $ toChar x

dataToCell :: Int -> Int -> Instruction
dataToCell a b = if (b >= 0 && b <= 128) then decodeInstruction a (chr b) else Reverse b

decodeInstruction :: Int -> Char -> Instruction
decodeInstruction _ ' ' = Empty
decodeInstruction _ '#' = Trampoline
decodeInstruction _ ';' = JumpOver
decodeInstruction _ 'j' = JumpForward
decodeInstruction _ '@' = Stop
decodeInstruction _ 'q' = Quit
decodeInstruction _ '+' = Add
decodeInstruction _ '*' = Multiply
decodeInstruction _ '-' = Subtract
decodeInstruction _ '/' = Divide
decodeInstruction _ '%' = Remainder
decodeInstruction _ '$' = Pop
decodeInstruction _ ':' = Duplicate
decodeInstruction _ '\\' = Swap
decodeInstruction _ 'n' = ClearStack
decodeInstruction _ 'x' = AbsoluteVector
decodeInstruction _ 'k' = Iterate
decodeInstruction _ '!' = LogicalNot
decodeInstruction _ '`' = GreaterThan
decodeInstruction _ '"' = StringMode
decodeInstruction _ '\'' = FetchCharacter
decodeInstruction _ 's' = StoreCharacter
decodeInstruction _ '?' = GoAway
decodeInstruction n '_' = If (1:replicate (n - 1) 0) (-1:replicate (n - 1) 0) '_'
decodeInstruction n '|' | n > 1 = If ([0, 1] ++ replicate (n - 2) 0) ([0, -1] ++ replicate (n - 2) 0) '|'
decodeInstruction n 'm' | n > 2 = If ([0, 0, -1] ++ replicate (n - 3) 0) ([0, 0, 1] ++ replicate (n - 3) 0) 'm'
decodeInstruction n 'w' | n > 1 = Compare
decodeInstruction n '[' | n > 1 = TurnLeft
decodeInstruction n ']' | n > 1 = TurnRight
decodeInstruction _ a | a >= '0' && a <= '9' = Push (ord a - ord '0') a
decodeInstruction _ a | a >= 'a' && a <= 'f' = Push (10 + ord a - ord 'a') a
decodeInstruction n '>' = Go (1:replicate (n - 1) 0) '>'
decodeInstruction n '<' = Go (-1:replicate (n - 1) 0) '<'
decodeInstruction n '^' | n > 1 = Go ([0, -1] ++ replicate (n - 2) 0) '^'
decodeInstruction n 'v' | n > 1 = Go ([0, 1] ++ replicate (n - 2) 0) 'v'
decodeInstruction n 'l' | n > 2 = Go ([0, 0, -1] ++ replicate (n - 3) 0) 'l'
decodeInstruction n 'h' | n > 2 = Go ([0, 0, 1] ++ replicate (n - 3) 0) 'h'
decodeInstruction _ 'P' = Print
decodeInstruction _ '{' = BeginBlock
decodeInstruction _ '}' = EndBlock
decodeInstruction _ 'u' = StackUnderStack
decodeInstruction _ 'g' = Get
decodeInstruction _ 'p' = Put
decodeInstruction _ '.' = OutputDecimal
decodeInstruction _ ',' = OutputCharacter
decodeInstruction _ '&' = InputDecimal
decodeInstruction _ '~' = InputCharacter
decodeInstruction _ c = Reverse (ord c)
