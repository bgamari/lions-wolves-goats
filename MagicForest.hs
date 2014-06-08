import Data.Foldable as F
import Data.List
import Data.Maybe (mapMaybe)
import Control.Monad (when, void)
import Control.Applicative
import qualified Data.Set as S
import Control.Monad.State.Strict
import Data.Semigroup
import System.Environment
import Debug.Trace
       
data Animal = Lion | Wolf | Goat
 
data Forest = Forest { lions, wolfs, goats :: !Int }
            deriving (Show, Ord, Eq)

canEat :: Animal -> Animal -> Maybe Animal
Lion `canEat` Wolf = Just Goat
Lion `canEat` Goat = Just Wolf
Wolf `canEat` Goat = Just Lion
_    `canEat` _    = Nothing


livesIn :: Animal -> Forest -> Int
Lion `livesIn` Forest l _ _ = l
Wolf `livesIn` Forest _ w _ = w
Goat `livesIn` Forest _ _ g = g

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

data Outcome a = More Forest [Outcome a]
               | Stable a
     
instance Functor Outcome where
  fmap f (More o xs) = More o $ fmap (fmap f) xs
  fmap f (Stable a)  = Stable (f a)

instance Foldable Outcome where
  foldMap f (More _ xs)  = foldMap (foldMap f) xs
  foldMap f (Stable a)   = f a

outcomes :: Forest -> Outcome Forest
outcomes f = 
   case mapMaybe ($ f) meals of
     []      -> Stable f
     forests -> More f $ map outcomes forests

meals :: [Forest -> Maybe Forest]
meals = [ Lion `eats` Wolf
        , Lion `eats` Goat
        , Wolf `eats` Goat
        ]

population :: Forest -> Int
population (Forest l w g) = l+w+g

maxStablePopulation :: Forest -> Int
maxStablePopulation = getMax . foldUniqueOutcomes (Max . population) . outcomes

foldUniqueOutcomes :: Monoid m => (a -> m) -> Outcome a -> m
foldUniqueOutcomes f o = evalState (go f o) S.empty
  where
    go :: Monoid m => (a -> m) -> Outcome a -> State (S.Set Forest) m
    go f (Stable a) = return $ f a
    go f (More o c) = do
      visited <- get
      if o `S.member` visited
        then return mempty
        else do modify $ S.insert o
                fold <$> mapM (go f) c

findStableForests :: Forest -> [Forest]
findStableForests f = S.toList $ last $ takeWhile devouringPossible $ iterate meal $ S.singleton f
  where
    meal :: S.Set Forest -> S.Set Forest
    meal = F.foldMap (\f->S.fromList $ mapMaybe ($ f) meals)
    devouringPossible :: S.Set Forest -> Bool
    devouringPossible = not . F.any (\f->null $ mapMaybe ($ f) meals)
    --devouringPossible = F.all (not . isStable)
    --devouringPossible = not . null
  
isStable :: Forest -> Bool
isStable (Forest 0 _ 0) = True
isStable (Forest _ 0 0) = True
isStable (Forest 0 0 _) = True
isStable _              = False

main = do
  [g,w,l] <- map read <$> getArgs
  let f = Forest {lions=l, wolfs=w, goats=g}
  --print $ maxStablePopulation f
  --print $ getSum $ foldUniqueOutcomes (const $ Sum 1) $ outcomes f
  print $ F.maximum $ map population $ findStableForests f
