import System.IO
import Data.List
import Control.Monad
import Data.Set
import Data.List
import Data.Map as Map

pokerHands = [(0,"None"),(1,"Pair"),(2,"Two Pair"),(3,"Three of a Kind"),(4,"Straight"),(5,"Flush"),(6,"Full House"),(7,"Four of a Kind"),(8,"Straight Flush")]
handNoSuit list  = [((x-1) `mod` 13)+1 | x<-list]
handToSuit list = [(x-1) `div` 13 | x<-list]
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
getReadableHand hand = [show (((x-1) `mod` 13)+1) ++ ["C","D","H","S","S"] !! ((x-1) `div` 13) | x<- hand]

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
    | head (handNoSuit(snd tup1)) == head (handNoSuit((snd tup2))) && length(snd tup1)==1 =[]
    | head (handNoSuit(snd tup1)) == head (handNoSuit(snd tup2)) = compareHighCards (fst tup1, (tail (snd tup1))) (fst tup2, (tail (snd tup2)))
    | (head (handNoSuit(snd tup1)) ==1) && (head (handNoSuit(snd tup2)))>1 = fst tup1
    | (head (handNoSuit(snd tup2))) ==1 && (head (handNoSuit(snd tup1)))>1 = fst tup2
    | head (handNoSuit(snd tup1)) > head (handNoSuit(snd tup2)) = fst tup1
    | head (handNoSuit(snd tup2)) > head (handNoSuit(snd tup1)) = fst tup2

compareHighSuits tup1 tup2 
    | head ((snd tup1)) == head (((snd tup2))) && length(snd tup1)==1 =[]
    | head ((snd tup1)) == head ((snd tup2)) = compareHighCards (fst tup1, (tail (snd tup1))) (fst tup2, (tail (snd tup2)))
    | head ((snd tup1)) > head ((snd tup2)) = fst tup1
    | head ((snd tup2)) > head ((snd tup1)) = fst tup2

getHandVal hand
    | (detectStraight (handNoSuit hand) == True)&& (detectFlush hand False 3 == True)=8
    | detectFlush hand False 3 == True = 5
    | detectStraight (handNoSuit hand)  == True = 4
    | otherwise = detectPairs (getPairs (handNoSuit hand) [])

separateHands list = ([snd x |x<-zip [0..9] list, (fst x)`mod`2==1 ]) : ([snd y| y<- zip [0..9] list, (fst y)`mod`2==0]) : []

getHandValName num = snd(pokerHands !! num)

makeHands list = (Hand {pokerNums=(sortBetter ((separateHands list) !! 0) []) , readableHand= getReadableHand (sortBetter((separateHands list) !! 0)[]), handVal = getHandVal ((separateHands list) !! 0), handValName = getHandValName (getHandVal ((separateHands list) !! 0))}) : (Hand {pokerNums=(sortBetter ((separateHands list) !! 1) []), readableHand= getReadableHand (sortBetter((separateHands list) !! 1)[]), handVal = getHandVal ((separateHands list) !! 1), handValName = getHandValName (getHandVal ((separateHands list) !! 1))}) :[]

compareHands listOfHands
    | handVal ( listOfHands !! 0) >handVal (listOfHands !! 1) = readableHand (listOfHands !! 0)
    | handVal ( listOfHands !! 1) >handVal (listOfHands !! 0) = readableHand (listOfHands !! 1)
    | compareHighCards (listToTup ((pokerNums (listOfHands !! 0)))) (listToTup ((pokerNums (listOfHands !! 1)))) /= [] = getReadableHand(compareHighCards (listToTup ((pokerNums (listOfHands !! 0)))) (listToTup ((pokerNums (listOfHands !! 1)))))
    | otherwise = getReadableHand(compareHighSuits (listToTup (pokerNums (listOfHands !! 0))) (listToTup (pokerNums (listOfHands !! 1))))

deal tenCards = compareHands(makeHands tenCards)

inject :: Int -> [Int] -> [Int]
inject x [] = [x] 
inject x (y:ys) = if ((x-1) `mod` 13)+1<= ((y-1) `mod`13)+1
                  then x:y:ys
                  else y : inject x ys

fromJust :: Maybe a -> a
fromJust (Just a) = a
sortBetter list acc
    | list == [] = acc
    | otherwise = sortBetter (tail list) (inject (head list) acc)



main = do

    putStrLn ("Result: "++show(deal [1, 40, 10, 49, 11, 50, 12, 51, 13, 52])++ " Correct Result: "++show(["1S", "10S", "11S", "12S", "13S"]))
    putStrLn ("Result: "++show(deal [1, 14, 2, 15, 3, 16, 4, 17, 5, 18])++ " Correct Result: "++show(["1D", "2D", "3D", "4D", "5D"]))
    putStrLn (  "Result: "++show(deal [2, 15, 3, 16, 4, 17, 5, 18, 6, 19])++ "Correct Result: "++show(["2D", "3D", "4D", "5D", "6D"]))
    putStrLn (  "Result: "++show(deal [3, 15, 4, 16, 5, 17, 6, 18, 7, 19])++ "Correct Result: "++show(["3C", "4C", "5C", "6C", "7C"]))
    putStrLn (  "Result: "++show(deal [1, 26, 2, 25, 3, 24, 4, 23, 5, 22])++ "Correct Result: "++show(["9D", "10D", "11D", "12D", "13D"]))
    putStrLn (  "Result: "++show(deal [26, 1, 25, 2, 24, 3, 23, 4, 22, 5])++ "Correct Result: "++show(["9D", "10D", "11D", "12D", "13D"]))


    putStrLn (  "Result: "++show(deal [1, 2, 14, 15, 27, 28, 40, 41, 3, 4])++ "Correct Result: "++show( ["1S", "1H", "1D", "1C", "3C"]))
    putStrLn (  "Result: "++show(deal [11, 12, 24, 25, 37, 38, 50, 51, 3, 4])++ "Correct Result: "++show(["4C", "12S", "12H", "12D", "12C"]))

    putStrLn (  "Result: "++show(deal [9, 10, 22, 23, 35, 36, 3, 4, 16, 17])++ "Correct Result: "++show( ["4D", "4C", "10H", "10D", "10C"]))
    putStrLn (  "Result: "++show(deal [2, 1, 15, 14, 28, 27, 3, 4, 16, 17])++ "Correct Result: "++show(["1H", "1D", "1C", "4D", "4C"]))

    putStrLn (  "Result: "++show(deal [2, 41, 4, 43, 6, 45, 7, 47, 10, 49])++ "Correct Result: "++show( ["2S", "4S", "6S", "8S", "10S"]))
    putStrLn (  "Result: "++show(deal [2, 41, 4, 43, 6, 45, 7, 46, 10, 49])++ "Correct Result: "++show(["2S", "4S", "6S", "7S", "10S"]))
    putStrLn (  "Result: "++show(deal [2, 41, 4, 42, 6, 45, 7, 46, 10, 49])++ "Correct Result: "++show(["2C", "4C", "6C", "7C", "10C"]))
    putStrLn (  "Result: "++show(deal [2, 41, 3, 42, 6, 45, 7, 46, 10, 49])++ "Correct Result: "++show(["2S", "3S", "6S", "7S", "10S"]))

    putStrLn (  "Result: "++show(deal [12, 25, 51, 38, 22, 9, 35, 48, 2, 15])++ "Correct Result: "++show( ["2C", "9H", "9D", "12S", "12C"]))
    putStrLn (  "Result: "++show(deal [12, 25, 51, 38, 22, 9, 35, 48, 2, 16])++ "Correct Result: "++show(["3D", "9S", "9C", "12H", "12D"]))
    putStrLn (  "Result: "++show(deal [12, 25, 51, 38, 22, 8, 35, 47, 2, 16])++ "Correct Result: "++show(["2C", "9H", "9D", "12S", "12C"]))
    putStrLn (  "Result: "++show(deal [12, 26, 51, 39, 22, 8, 35, 47, 2, 16])++ "Correct Result: "++show(["3D", "8S", "8C", "13H", "13D"]))

    putStrLn (  "Result: "++show(deal [13, 26, 52, 39, 9, 22, 8, 47, 7, 33])++ "Correct Result: "++show(["7C", "8C", "9C", "13S", "13C"]))
    putStrLn (  "Result: "++show(deal [13, 26, 52, 39, 9, 22, 8, 47, 6, 33])++ "Correct Result: "++show(["7H", "8S", "9D", "13H", "13D"]))
    putStrLn (  "Result: "++show(deal [13, 26, 52, 39, 9, 22, 7, 21, 6, 19])++ "Correct Result: "++show(["6D", "8D", "9D", "13H", "13D"]))
    putStrLn (  "Result: "++show(deal [13, 26, 52, 39, 9, 11, 7, 20, 6, 19])++ "Correct Result: "++show(["6D", "7D", "11C", "13H", "13D"]))
    putStrLn (  "Result: "++show(deal [13, 25, 52, 38, 9, 11, 7, 20, 6, 19])++ "Correct Result: "++show(["6C", "7C", "9C", "13S", "13C"]))

    putStrLn (  "Result: "++show(deal [1, 40, 16, 29, 5, 44, 8, 21, 12, 25])++ "Correct Result: "++show( ["1S", "3H", "5S", "8D", "12D"]))
    putStrLn (  "Result: "++show(deal [1, 40, 15, 29, 5, 44, 8, 21, 12, 25])++ "Correct Result: "++show(["1S", "3H", "5S", "8D", "12D"]))
    putStrLn (  "Result: "++show(deal [1, 40, 15, 29, 4, 44, 8, 21, 12, 25])++ "Correct Result: "++show(["1S", "3H", "5S", "8D", "12D"]))
    putStrLn (  "Result: "++show(deal [1, 40, 15, 29, 4, 44, 7, 21, 12, 25])++ "Correct Result: "++show(["1S", "3H", "5S", "8D", "12D"]))
    putStrLn (  "Result: "++show(deal [1, 40, 15, 29, 4, 44, 7, 21, 11, 25])++ "Correct Result: "++show(["1S", "3H", "5S", "8D", "12D"]))
    putStrLn (  "Result: "++show(deal [1, 39, 15, 29, 4, 44, 7, 21, 12, 25])++ "Correct Result: "++show(["1C", "2D", "4C", "7C", "12C"]))