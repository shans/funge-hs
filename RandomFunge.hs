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


