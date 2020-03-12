import System.IO
import Data.List
import Data.Set

pokerHands = [(0,"None"),(1,"Pair"),(2,"Two Pair"),(3,"Three of a Kind"),(4,"Straight"),(5,"Flush"),(6,"Full House"),(7,"Four of a Kind"),(8,"Straight Flush")]
handNoSuit list  = [x `mod` 13 | x<-list]
handToSuit list = [x `div` 13 | x<-list]
compareFirstTwo x1 x2 = x2 - x1 ==1

data Hand = Hand {pokerNums :: [Int]
                , readableHand :: [String]
                , handVal :: Int
                } deriving (Eq, Show, Read)


has [] _ = False
has (x:xs) a
  | x == a    = True
  | otherwise = has xs a

unique [] = []
unique (x:xs)
  | has xs x  = unique xs
  | otherwise = x : unique xs

detectStraight hand = isStraight (handNoSuit hand) True

isStraight (x:xs) condition
    | condition == False = False
    | xs==[] = condition
    | otherwise = isStraight (xs) (compareFirstTwo x (head xs))

detectFlush list condition num
    | condition == True = True
    | num == -1 = condition
    | (condition == False) && (num <=3) = detectFlush list (isFlush list num) (num-1)

isFlush list num
    | length [x | x<- handToSuit list, x==num] == 5 = True
    | otherwise = False


multiTable = [[x*y| x<-[1..10]]|y<-[1..10]]
handToTup hand =zip [0..4] hand

occurences num list = [x| x<- unique list, x==num]



getPairs (x:xs) acc
    | xs==[] = acc
    | otherwise = getPairs xs (acc++ (occurences x xs))

detectPairs list
    | (unique list == list) && (length list ==1) = 1
    | (unique list == list) && (length list ==2) = 2
    | (length list == 3) && length (unique list)==2 = 6
    | (length list == 3) && length (unique list)==1 = 7
    | otherwise = 0

compareHighCards tup1 tup2

    | head (snd tup1) == head (snd tup2) = compareHighCards (fst tup1, (tail (snd tup1))) (fst tup2, (tail (snd tup2)))
    | head (snd tup1) > head (snd tup2) = fst tup1
    | head (snd tup2) > head (snd tup1) = fst tup2
    | snd tup1==[] = False


compareHighSuits tup1 tup2 = compareHighCards (fst tup1, (handToSuit (snd tup1))) (fst tup2, (handToSuit (snd tup2)))

getHandVal hand
    | (isStraight hand True == True)&& (detectFlush hand False 3 == True)=8
    | detectFlush hand False 3 == True = 5
    | detectStraight hand  == True = 4
    | otherwise = detectPairs (getPairs (handNoSuit hand) [])