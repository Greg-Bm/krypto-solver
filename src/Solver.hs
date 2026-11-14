{-# LANGUAGE ScopedTypeVariables #-}
module Solver (solve, app) where

import Expr
import EquivRep
import Relation
import Control.Monad
import System.IO
import System.Environment
import Control.Exception

solve nums goal = do
  let exprs = gen nums
  let solves = filter ((== Just goal) . eval) exprs
  let reducedSolves = equivRep relation solves
  reducedSolves

app = do
  args <- getArgs
  app' args `catch` exit

app' args = do
  let [numsStr, goalStr] = args
  let nums = read numsStr
  let goal = read goalStr
  let s = solve nums (fromIntegral goal)
  let n = length s
  putStrLn $ show n <> " unique solutions found."
  forM_ s print

exit :: SomeException -> IO ()
exit e = do
  putStrLn "An internal error occured, possibly due to incorrect input."
  putStrLn "The first argument should be the list of (whole) numbers, and the second should be the goal."
  putStrLn "For example, `krypto-solver [1,2,3,4] 24`"
  putStrLn ""
  putStrLn $ show e
