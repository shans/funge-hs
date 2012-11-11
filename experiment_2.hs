import FungeBase
import RunFunge
import RandomFunge
import Control.Monad
import Control.DeepSeq
import Data.Maybe
import Data.List

import Data.IORef
import System.Posix.Signals

run_1_core dim n p = doRandomFunge (runFor n) [dim] [0] [1] (defaultConfig { charSet = chars1D, trace = noTrace, acceptUserInput = False, activeExtensions = "" , printer = p })

run_2_core dim n p = doRandomFunge (runFor n) [dim, dim] [0, 0] [1, 0] (defaultConfig { charSet = chars2D, trace = noTrace, acceptUserInput = False, activeExtensions = "", printer = p, randomizer = loadRandomFunge', displayOutput = False })

run_3_core dim n p = doRandomFunge (runFor n) [dim, dim, dim] [0, 0, 0] [1, 0, 0] (defaultConfig { charSet = chars3D, trace = noTrace, acceptUserInput = False, activeExtensions = "", printer = p })

go core dim n runs = do
  currentProgram <- newIORef ""
  installHandler sigINT (CatchOnce (do { p <- readIORef currentProgram; putStrLn ("killed while executing " ++ p); raiseSignal sigINT})) Nothing  
  --result <- sequence . map (\(a,b) -> do {putStrLn (show b) ; a}) $ zip (replicate runs $ core dim n (\a b -> writeIORef currentProgram (funge2DToString a b))) [0..(runs - 1)]
  let resultToTerminators = sort. map snd . catMaybes . map fst
  let terminatorsToCounts = map (\a -> (head a, length a)) . group
  let resultToCounts = terminatorsToCounts . resultToTerminators
  let monads = map (\(a,b) -> do {putStrLn (show b) ; a}) $ zip (replicate runs $ core dim n (\a b -> writeIORef currentProgram (funge2DToString a b))) [0..(runs - 1)]
  terminators <- liftM resultToTerminators $ oneAtATime monads
  let counts = terminatorsToCounts terminators
  putStrLn "----"
  putStrLn ""
  sequence_ $ map (\(val, num) -> putStrLn (show val ++ ": " ++ show (fromIntegral num / fromIntegral runs * 100.0))) counts
  putStrLn ("~: " ++ show (fromIntegral (runs - length terminators) / fromIntegral runs * 100.0))

oneAtATime [] = return []
oneAtATime (h:t) = do
  r <- h
  -- putStrLn $ show r
  t' <- r `deepseq` oneAtATime t
  return $ r:t'

{-- 

main = go run_1_core 10 100 100000

main = go run_1_core 100 1000 100000

main = go run_1_core 1000 1000 100000

main = go run_1_core 10 1000 100000

main = go run_2_core 10 1000 100000

main = go run_2_core 3 1000 100000

main = go run_2_core 32 1000 100000

main = go run_2_core 100 1000 100000

--}
