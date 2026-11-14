module KSBench (main) where
import Solver (solve)

import Control.Monad ( replicateM, replicateM_ )
import Test.Tasty (localOption)
import Test.Tasty.Bench
    ( bench, bgroup, defaultMain, nf, TimeMode(WallTime) )
import Data.Foldable (traverse_)

main :: IO ()
main = defaultMain
    [ localOption WallTime $ bgroup "5 numbers"
        [ bench "different" $ nf (solve [1,2,3,4,5]) 24,
          bench "same" $ nf (solve [3,3,3,3,3]) 24
        ],
      localOption WallTime $ bgroup "$ numbers"
        [ bench "different" $ nf (solve [1,2,3,4]) 24,
          bench "same" $ nf (solve [3,3,3,3]) 24
        ]
    ]
