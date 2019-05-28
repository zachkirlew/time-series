import Data.List
import qualified Data.Map as M
import Data.Semigroup
import Data.Maybe
import TimeSeriesData

data TS a = TS [Int] [Maybe a]

-- Create time series from files
createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues where
    completeTimes = [minimum times .. maximum times]
    timeValueMap = M.fromList (zip times values)
    extendedValues = map (`M.lookup` timeValueMap) completeTimes

fileToTS :: [(Int, a)] -> TS a 
fileToTS file = createTS times values where
    (times, values) = unzip file

showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time (Just value) = mconcat [show time, "| ", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "| NA\n"]

instance Show a => Show (TS a) where
    show (TS times values) = mconcat rows where
        rows = zipWith showTVPair times values

-- Time series
ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

-- Combining time series 
insertMaybePair :: Ord k => M.Map k v -> (k, Maybe v) -> M.Map k v
insertMaybePair map (_, Nothing) = map
insertMaybePair map (key, Just val) = M.insert key val map

combineTS :: TS a -> TS a -> TS a
combineTS ts1 (TS [] []) = ts1
combineTS (TS [] []) ts2 = ts2
combineTS (TS t1 v1) (TS t2 v2) = TS allTimes combinedValues where
    times = mconcat [t1, t2]
    allTimes = [minimum times .. maximum times]
    tvMap = foldl' insertMaybePair M.empty (zip t1 v1)
    updatedMap = foldl' insertMaybePair tvMap (zip t2 v2)
    combinedValues = map (`M.lookup` updatedMap) allTimes

tsAll :: TS Double
tsAll = mconcat [ts1,ts2,ts3,ts4]

instance Semigroup (TS a) where
    (<>) = combineTS

instance Monoid (TS a) where
    mempty = TS [] []
    mappend = (<>)

-- Calculations on time series
mean :: (Real a) => [a] -> Double
mean xs = total / count where
    total = (realToFrac.sum) xs
    count = (realToFrac.length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values)
    | all isNothing values = Nothing
    | otherwise =  Just average where
        average = mean (catMaybes values)

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a 
makeTSCompare compareFunc = newFunc where
    newFunc (i1, Nothing) (_, Nothing) = (i1, Nothing)
    newFunc (_, Nothing) (i, val) = (i, val)
    newFunc (i, val) (_, Nothing) = (i, val)
    newFunc (i1, Just val1) (i2, Just val2) 
        | compareFunc val1 val2 == val1 = (i1, Just val1)
        | otherwise = (i2, Just val2)

compareTS :: Eq a => CompareFunc a -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values)
    | all isNothing values = Nothing
    | otherwise = Just best where
        pairs = zip times values
        best = foldl' (makeTSCompare func) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

-- Time series transformations
diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing:diffValues) where
    diffValues = zipWith diffPair (tail values) values

meanIncrease :: Maybe Double
meanIncrease = meanTS (diffTS tsAll)

meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals
    | any isNothing vals = Nothing
    | otherwise = Just avg where
        avg = mean (map fromJust vals)

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n = []
movingAvg vals n
    | length nextVals == n = meanMaybe nextVals:movingAvg restVals n
    | otherwise =  [] where
        nextVals = take n vals
        restVals = tail vals

movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) n= TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues where
    ma = movingAvg values n
    nothings = replicate (n `div` 2) Nothing
    smoothedValues = mconcat [nothings,ma,nothings]