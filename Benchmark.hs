import Criterion
import Criterion.Main
import System.Process

data Forest = Forest { lions, wolfs, goats :: Int }
            deriving (Show)

population :: Forest -> Int
population (Forest l w g) = l+w+g

forestArgs :: Forest -> [String]
forestArgs (Forest l w g) = [show l, show w, show g]

js :: Forest -> IO ()
js f = callProcess "js" args
  where args = "magicForest.js" : forestArgs f

hs :: Forest -> IO ()
hs f = callProcess "./MagicForest" (forestArgs f)

cpp :: Forest -> IO ()
cpp f = callProcess "./magic-forest-cpp" (forestArgs f)

forests :: [Forest]
forests =
  [ Forest (6+n) (55+n) (17+n)
    | n <- [0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000]
  ]

benchmarksFor :: Forest -> Benchmark
benchmarksFor f = bgroup (show $ population f)
  [ bench "C++" (cpp f)
  , bench "Haskell" (hs f)
  , bench "Javascript" (js f)
  ]
  
main = defaultMain $ map benchmarksFor forests
  
