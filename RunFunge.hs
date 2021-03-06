module RunFunge where

import FungeBase
import Funge
import RandomFunge
import Control.Monad.State
import Control.Monad.Reader

defaultConfig = Config { charSet = chars2D , trace = ipStackInstructionBoundsTrace , printer = show2DFunge , acceptUserInput = True, activeExtensions = "P", randomizer = loadRandomFunge, displayOutput = True }

run :: VMRunner Int
run = do
  trace <- config getTrace 
  trace
  stop <- step
  case stop of
    Nothing -> run
    Just x -> return x

runFor :: Int -> VMRunner (Maybe (Int, Int), [[Int]])
runFor n = runFor' n where
  runFor' 0 = do { s <- getStack; return (Nothing, s) }
  runFor' l = do
    trace <- config getTrace 
    trace
    stop <- step
    case stop of
      Nothing -> runFor' (l - 1)
      Just x -> do { s <- getStack; return (Just (x, n - l + 1), s) }

ic pos dir = VM emptyMap [] [] (pos, dir, pos) False Nothing

loadAndDo :: VMRunner a -> String -> [Int] -> [Int] -> [Int] -> Config -> IO a
loadAndDo exec s dim pos dir c = do
  let program = runReaderT (runStateT (do { input s (map (\_ -> 0) dim) dim ; exec }) (ic pos dir)) c
  r <- program
  return (fst r)

doRandomFunge :: VMRunner a -> [Int] -> [Int] -> [Int] -> Config -> IO a
doRandomFunge exec dim pos dir c = do
  let program = runReaderT (runStateT (do { r <- config getRandomizer; r dim ; exec}) (ic pos dir)) c
  r <- program
  return (fst r)

-- runs the "random" funge generated by the provided seed
doSpecifiedRandomFunge :: String -> VMRunner a -> [Int] -> [Int] -> [Int] -> Config -> IO a
doSpecifiedRandomFunge seed exec dim pos dir c = do
  let program = runReaderT (runStateT (do { randomFungeFromSeed seed dim; exec}) (ic pos dir)) c
  r <- program
  return (fst r)
