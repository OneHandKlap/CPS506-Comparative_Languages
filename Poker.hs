import System.IO
import Data.List
import Control.Monad
import Data.Set
import Data.List

pokerHands = [(0,"None"),(1,"Pair"),(2,"Two Pair"),(3,"Three of a Kind"),(4,"Straight"),(5,"Flush"),(6,"Full House"),(7,"Four of a Kind"),(8,"Straight Flush")]
handNoSuit list  = [((x-1) `mod` 13)+1 | x<-list]
handToSuit list = [x `div` 13 | x<-list]
compareFirstTwo x1 x2 = x2 - x1 ==1

data Hand = Hand {pokerNums :: [Int]
                , readableHand :: [String]
                , handVal :: Int
                , handValName :: String
                } deriving (Eq, Show, Read)

has [] _ = False
has (x:xs) a
  | x == a    = True
  | otherwise = has xs a

unique [] = []
unique (x:xs)
  | has xs x  = unique xs
  | otherwise = x : unique xs

listToTup list = (list,list)
getReadableHand hand = [show (((x-1) `mod` 13)+1) ++ ["H","C","D","S","S"] !! ((x+1) `div` 13) | x<- hand]

detectStraight hand = isStraight (sort(handNoSuit hand)) True

isStraight (x:xs) condition
    | ((xs==[10,11,12,13])&&(x==1)) == True=True
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
    | (head (snd tup1) == head (snd tup2)) && length(snd tup1)==1 =[]
    | head (snd tup1) == head (snd tup2) = compareHighCards (fst tup1, (tail (snd tup1))) (fst tup2, (tail (snd tup2)))
    | head (snd tup1) > head (snd tup2) = fst tup1
    | head (snd tup2) > head (snd tup1) = fst tup2

compareHighSuits tup1 tup2 = compareHighCards (fst tup1, (handToSuit (snd tup1))) (fst tup2, (handToSuit (snd tup2)))

getHandVal hand
    | (detectStraight (handNoSuit hand) == True)&& (detectFlush hand False 3 == True)=8
    | detectFlush hand False 3 == True = 5
    | detectStraight (handNoSuit hand)  == True = 4
    | otherwise = detectPairs (getPairs (handNoSuit hand) [])

separateHands list = ([snd x |x<-zip [0..9] list, (fst x)`mod`2==1 ]) : ([snd y| y<- zip [0..9] list, (fst y)`mod`2==0]) : []

getHandValName num = snd(pokerHands !! num)

makeHands list = (Hand {pokerNums=((separateHands list) !! 0), readableHand= getReadableHand ((separateHands list) !! 0), handVal = getHandVal ((separateHands list) !! 0), handValName = getHandValName (getHandVal ((separateHands list) !! 0))}) : (Hand {pokerNums=((separateHands list) !! 1), readableHand= getReadableHand ((separateHands list) !! 1), handVal = getHandVal ((separateHands list) !! 1), handValName = getHandValName (getHandVal ((separateHands list) !! 1))}) :[]

compareHands listOfHands
    | handVal ( listOfHands !! 0) >handVal (listOfHands !! 1) = readableHand (listOfHands !! 0)
    | handVal ( listOfHands !! 1) >handVal (listOfHands !! 0) = readableHand (listOfHands !! 1)
    | compareHighCards (listToTup (pokerNums (listOfHands !! 0))) (listToTup (pokerNums (listOfHands !! 1))) /= [] = getReadableHand (compareHighCards (listToTup (pokerNums (listOfHands !! 0))) (listToTup (pokerNums (listOfHands !! 1))))
    | otherwise = getReadableHand(compareHighSuits (listToTup (pokerNums (listOfHands !! 0))) (listToTup (pokerNums (listOfHands !! 1))))

deal tenCards = sort(compareHands(makeHands tenCards))


main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ (deal (read l :: [Int]))