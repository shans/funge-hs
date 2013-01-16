module RandomFunge where

import FungeBase
import Funge
import System.Random
import Control.Monad
import Control.Monad.State

-- this is the set of single-threaded instructions that doesn't require input from the user
chars3D = " #;j@q+*-/%$:\\nxk!`\"'s?_|mw[]0123456789abcdef><^vlh{}ugp.,r"

chars2D = " #;j@q+*-/%$:\\nxk!`\"'s?_|w[]0123456789abcdef><^v{}ugp.,r"

chars1D = " #;j@q+*-/%$:\\nxk!`\"'s?_w[]0123456789abcdef><{}ugp.,r"

randomChar charSet = do
  r <- randomIO
  return $ charSet !! (r `mod` length charSet)

loadRandomFunge :: [Int] -> VMRunner ()
loadRandomFunge dim = do
  charSet <- config getCharSet
  extensions <- config getActiveExtensions
  field <- io $ randomFunge' (charSet ++ extensions) dim
  vis <- config getPrinter
  io $ vis dim field
  input field (replicate (length dim) 0) dim
  where
    randomFunge' charSet [l] = replicateM l (randomChar charSet)
    randomFunge' charSet (h:t) = liftM concat . replicateM h $ randomFunge' charSet t

loadRandomFunge' :: [Int] -> VMRunner ()
loadRandomFunge' dim = do
  charSet <- config getCharSet
  extensions <- config getActiveExtensions
  let opSet = map (decodeInstruction (length dim)) (charSet ++ extensions)
  seed <- io $ getStdGen
  io $ putStrLn (show seed)
  m <- io $ randomFunge' opSet dim (replicate (length dim) 0) [] emptyMap
  putInstructionMap m
  putBounds (map (\a -> (0, a)) dim)
    
randomFunge' :: [Instruction] -> [Int] -> [Int] -> [Int] -> InstructionMap -> IO InstructionMap
randomFunge' _ [0] _ _ map = return map
randomFunge' ops [l] [pos] rest map = randomFunge' ops [l - 1] [pos + 1] rest $ updateWithAction (rest ++ [pos]) (randomOp ops) map 
randomFunge' _ (0:t) _ _ map = return map
randomFunge' ops (h:t) (ph:pt) rest map = do { r <- randomFunge' ops t pt (ph:rest) map; randomFunge' ops ((h-1):t) ((ph+1):pt) rest r }

randomOp :: [Instruction] -> IO Instruction
randomOp opSet = do
  r <- randomIO
  return $ opSet !! (r `mod` length opSet)

randomFungeFromSeed :: String -> [Int] -> VMRunner ()
randomFungeFromSeed seed dim = do
  charSet <- config getCharSet
  extensions <- config getActiveExtensions
  let opSet = map (decodeInstruction (length dim)) (charSet ++ extensions)
  io $ setStdGen (read seed)
  m <- io $ randomFunge' opSet dim (replicate (length dim) 0) [] emptyMap
  putInstructionMap m
  putBounds (map (\a -> (0, a)) dim)
