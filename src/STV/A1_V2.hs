module STV.A1_V2 where
import Data.List
import Data.Map (fromListWith, toList)
import Data.Maybe (listToMaybe)
import Data.List (sortBy)
import Data.Function (on)
import Clean.CleanVotes



-- length of votes
lengthVotes :: Int
lengthVotes = length cleanVotes

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdupsAlt :: Eq a => [a] -> [a]
rmdupsAlt []     = []
rmdupsAlt (x:xs) = x : filter (/= x) (rmdupsAlt xs)


result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdupsAlt vs]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c]    -> c
                (c:cs) -> winner' (elim c bs)

numSeats :: Double
numSeats = 3


quota :: Double
quota = ((realToFrac lengthVotes) / (realToFrac(numSeats + 1))) + 1.0

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

-- to be able to use each column individually 

colN :: Int -> [[a]] -> [a]
colN n matrix = transpose matrix !! n

-- [(1, count), (2, count), (3, count), (4, count), (5, count)] for each candidate
-- colN 0 taking the first column of cleanVotes --> Abbott's votes
-- getting the frequency of each distinct value in the column (votes) 
-- drop the first value as it's the *

abbott :: [(String, Int)]
abbott = [((show y), snd (frequency (colN 0 cleanVotes)!! y) ) | y <- [1..5]]

balls :: [(String, Int)]
balls = [((show y), snd (frequency (colN 1 cleanVotes)!! y) ) | y <- [1..5]]

burbhm :: [(String, Int)]
burbhm= [((show y), snd (frequency (colN 2 cleanVotes)!! y) ) | y <- [1..5]]

dMill:: [(String, Int)]
dMill =  [((show y), snd (frequency (colN 3 cleanVotes)!! y) ) | y <- [1..5]]

eMill :: [(String, Int)]
eMill =  [((show y), snd (frequency (colN 4 cleanVotes)!! y) ) | y <- [1..5]]


mySort :: Ord b => [(a, b)] -> [(a, b)]
mySort = sortBy (flip compare `on` snd)

no1 :: (String, Int)
no1 = (winner' finalVotes, winnerCount)

nameNo1 :: String
nameNo1 = fst (no1)

--- first round
-- get winner index value
winnerIndex :: Int
winnerIndex = (elemIndices (winner' finalVotes) candidates) !! 0

winnerCount :: Int
winnerCount = (count ("1") ([(cleanVotes!!y!!winnerIndex)| y <-[0..(lengthVotes-1)]]))

surplus :: Fractional a => a -> a
surplus candCount = candCount - (realToFrac quota)

--get transferable votes count (winner = 1, other = 2)

transferable :: Int -> [(String, String)]
transferable indx
  | indx == winnerIndex = [(cleanVotes!!y!!winnerIndex, cleanVotes!!y!!z) | y <-[0..265], z<-[0..4], z /= winnerIndex]
  | otherwise = [(cleanVotes!!y!!indx, cleanVotes!!y!!z) | y <-[0..265], z<-[0..4], z /= winnerIndex, z/= indx]

transferableVotes :: String -> String -> Int -> Int
transferableVotes pos1 pos2 indx = count (pos1, pos2) (transferable indx)

-- getting each candidates votes where winner is first pref
abbottsnd :: Int -> [(String, String)]
abbottsnd elecIndex = [(cleanVotes!!y!!elecIndex, cleanVotes!!y!!0) | y <-[0..(lengthVotes-1)]]

burbhmsnd :: Int -> [(String, String)]
burbhmsnd elecIndex = [(cleanVotes!!y!!elecIndex, cleanVotes!!y!!1) | y <-[0..(lengthVotes-1)]]

ballssnd :: Int -> [(String, String)]
ballssnd elecIndex = [(cleanVotes!!y!!elecIndex, cleanVotes!!y!!2) | y <-[0..(lengthVotes-1)]]

emillsnd :: Int -> [(String, String)]
emillsnd elecIndex = [(cleanVotes!!y!!elecIndex, cleanVotes!!y!!4) | y <-[0..(lengthVotes-1)]]

dmillsnd :: Int -> [(String, String)]
dmillsnd elecIndex = [(cleanVotes!!y!!elecIndex, cleanVotes!!y!!3) | y <-[0..(lengthVotes-1)]]

--getting each candidates votes where winner is first pref and candidate is second pref etc.
sndprefVotes :: [[Double]]
sndprefVotes = [myFunc2 abbott (abbottsnd winnerIndex) "1" "2" (weightOrig "1" "2" (realToFrac winnerCount)  winnerIndex) , myFunc2 balls (ballssnd winnerIndex)  "1" "2" (weightOrig "1" "2" (realToFrac winnerCount)  winnerIndex), myFunc2 burbhm (burbhmsnd winnerIndex)  "1" "2" (weightOrig "1" "2" (realToFrac winnerCount)  winnerIndex), myFunc2 dMill (dmillsnd winnerIndex)  "1" "2" (weightOrig "1" "2" (realToFrac winnerCount)  winnerIndex), myFunc2 eMill (emillsnd winnerIndex)  "1" "2" (weightOrig "1" "2" (realToFrac winnerCount)  winnerIndex)]

thrprefVotes :: [[Double]]
thrprefVotes = [myFunc2 abbott (abbottsnd no2Index) "2" "3" (weightOrig "2" "3"  countNo2  no2Index), myFunc2 balls (ballssnd no2Index) "2" "3" (weightOrig "2" "3"  countNo2  no2Index),myFunc2 burbhm (burbhmsnd no2Index)"2" "3" (weightOrig "2" "3"  countNo2  no2Index), myFunc2 dMill (dmillsnd no2Index) "2" "3" (weightOrig "2" "3"  countNo2  no2Index), myFunc2 eMill (emillsnd no2Index) "2" "3" (weightOrig "2" "3"  countNo2  no2Index)]

weightOrig :: (Real a1, Fractional a2, Fractional a1) => String -> String -> a1 -> Int -> a2
weightOrig pos1 pos2 candCount indx = 1000 * ((realToFrac (surplus candCount)) / (1000 * (realToFrac (transferableVotes pos1 pos2 indx) ) ) )

weight2  :: (Real a1, Fractional a2, Fractional a1) =>     String -> String -> a1 -> Int -> a2
weight2 pos1 pos2 candCount indx= (weightOrig "1" "2" (realToFrac winnerCount)  winnerIndex) * ((realToFrac (surplus candCount)) / ((weightOrig "1" "2" (realToFrac winnerCount)  winnerIndex) * (realToFrac (transferableVotes pos1 pos2 indx) ) ) )

myFunc2  :: (Fractional a1, Real a2, Eq a3, Eq b) =>    [(a4, a2)] -> [(a3, b)] -> a3 -> b -> a1 -> [a1]
myFunc2 name1 name2  pos1 pos2 newWeight = [ newWeight  * (realToFrac (count(pos1, pos2) name2)) + (realToFrac (snd(name1 !! 0)))]

no2PassQuota :: [(String, [Double])]
no2PassQuota = [((candidates !! y), (sndprefVotes !! y))  | y <- [0..4], y /= winnerIndex, ((sndprefVotes !! y)!!0) > (realToFrac quota)]

no2 :: (String, [Double])
no2 = head(mySort no2PassQuota)

nameNo2 :: String
nameNo2 = fst (no2 )

countNo2 :: Double
countNo2 = realToFrac((snd no2) !! 0)

no2Index :: Int
no2Index = (elemIndices ((filter (`elem` [nameNo2]) candidates)!!0) candidates) !! 0

no3PassQuota :: [(String, [Double])]
no3PassQuota = [((candidates !! y), (thrprefVotes !! y))  | y <- [0..4], y /= winnerIndex, y /= no2Index]

no3 :: (String, [Double])
no3 = head(mySort no3PassQuota)

nameNo3 :: String
nameNo3 = fst (no3 )

countNo3 :: [Double]
countNo3 = snd no3

no3Index :: Int
no3Index = (elemIndices ((filter (`elem` [nameNo3]) candidates)!!0) candidates) !! 0

no4 :: (String, [Double])
no4 
  | numSeats == 4 = ((mySort no3PassQuota) !! 1)
  | otherwise = (("No fourth seat: filled seats (" ++ (show numSeats) ++ ")"),[]) 

nameNo4 :: String
nameNo4 = fst no4

countNo4 :: [Double]
countNo4 = snd no4

no4Index :: Int
no4Index = (elemIndices ((filter (`elem` [nameNo4]) candidates)!!0) candidates) !! 0

eliminated :: (String, [Double])
eliminated 
  | numSeats == 3 = last (mySort no3PassQuota)
  | (countNo4 !! 0 ) >= quota = ("Eliminated: ", [])



