import Data.Foldable as F
import Data.Maybe (mapMaybe)
import Control.Applicative
import qualified Data.Set as S
import System.Environment

data Animal = Lion | Wolf | Goat
 
data Forest = Forest { lions, wolfs, goats :: !Int }
            deriving (Show, Ord, Eq)

-- | Can a one animal eat another and, if so, what will it become?
canEat :: Animal -> Animal -> Maybe Animal
Lion `canEat` Wolf = Just Goat
Lion `canEat` Goat = Just Wolf
Wolf `canEat` Goat = Just Lion
_    `canEat` _    = Nothing

-- | The population of the given critter in a forest
livesIn :: Animal -> Forest -> Int
Lion `livesIn` Forest l _ _ = l
Wolf `livesIn` Forest _ w _ = w
Goat `livesIn` Forest _ _ g = g

-- | @eats a b f@ is the forest that results from animal @a@ eating
-- animal @b@ in forest @f@
eats :: Animal -> Animal -> Forest -> Maybe Forest
eats a b f
  | a `livesIn` f == 0     = Nothing
  | b `livesIn` f == 0     = Nothing
  | Just c <- a `canEat` b = Just $ add c $ sub a $ sub b f
  | otherwise              = Nothing
  where
    mod :: Int -> Animal -> Forest -> Forest
    mod n Lion (Forest l w g) = Forest (l+n)  w     g
    mod n Wolf (Forest l w g) = Forest  l    (w+n)  g
    mod n Goat (Forest l w g) = Forest  l     w    (g+n)

    sub, add :: Animal -> Forest -> Forest
    sub = mod (-1)
    add = mod   1

-- | The possible meals
meals :: [Forest -> Maybe Forest]
meals = [ Lion `eats` Wolf
        , Lion `eats` Goat
        , Wolf `eats` Goat
        ]

-- | The overall population of a forest
population :: Forest -> Int
population (Forest l w g) = l+w+g

-- | A list of generations of a forest's evolution
evolve :: Forest -> [S.Set Forest]
evolve = iterate step . S.singleton
  where
    step :: S.Set Forest -> S.Set Forest
    step = F.foldMap (\f->S.fromList $ mapMaybe ($ f) meals)

-- | Is a forest stable?
stable :: Forest -> Bool
stable f = null $ mapMaybe ($ f) meals

-- | A list of the largest stable forests. Specifically, only those
-- from the first stable generation
largestStableForests :: Forest -> [Int]
largestStableForests =
    S.toDescList . S.map population
    . head . dropWhile S.null
    . map (S.filter stable) . evolve

main = do
  [g,w,l] <- map read <$> getArgs
  let f = Forest {lions=l, wolfs=w, goats=g}
  print $ head $ largestStableForests f
