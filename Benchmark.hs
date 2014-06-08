import Criterion
import Criterion.Main
import System.Process
import Control.Monad (void)

data Forest = Forest { lions, wolfs, goats :: Int }
            deriving (Show)

population :: Forest -> Int
population (Forest l w g) = l+w+g

forestArgs :: Forest -> [String]
forestArgs (Forest l w g) = [show l, show w, show g]

js :: Forest -> IO ()
js f = void $ readProcess "js" args ""
  where args = "magicForest.js" : forestArgs f

hs :: Forest -> IO ()
hs f = void $ readProcess "./MagicForest" (forestArgs f) ""

cpp :: Forest -> IO ()
cpp f = void $ readProcess "./magic_forest" (forestArgs f) ""

forests :: [Forest]
forests =
  [ Forest (6+n) (55+n) (17+n)
    | n <- [0, 100, 200, 300]
  ]

benchmarksFor :: Forest -> Benchmark
benchmarksFor f = bgroup (show $ population f)
  [ bench "C++" (cpp f)
  , bench "Haskell" (hs f)
  , bench "Javascript" (js f)
  ]
  
main = defaultMain $ map benchmarksFor forests
  
