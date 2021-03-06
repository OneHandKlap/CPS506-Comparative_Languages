{-|
Partner: Yu Xing Wu 500901559
-}
module Poker where

import System.IO
import Data.List
import Control.Monad
import Data.Set
import Data.List
import Data.Map as Map

pokerHands = [(0,"None"),(1,"Pair"),(2,"Two Pair"),(3,"Three of a Kind"),(4,"Straight"),(5,"Flush"),(6,"Full House"),(7,"Four of a Kind"),(8,"Straight Flush")]

--A deck containing all 52 cards for Poker
pokerDeck = [(0, "1C"),
    (1,"1C"), (2,"2C"), (3,"3C"), (4,"4C"), (5,"5C"), (6,"6C"), (7,"7C"), (8,"8C"), (9,"9C"),(10,"10C"),(11,"11C"),(12,"12C"),(13,"13C"),
    (14,"1D"),(15,"2D"),(16,"3D"),(17,"4D"),(18,"5D"),(19,"6D"),(20,"7D"),(21,"8D"),(22,"9D"),(23,"10D"),(24,"11D"),(25,"12D"),(26,"13D"),
    (27,"1H"),(28,"2H"),(29,"3H"),(30,"4H"),(31,"5H"),(32,"6H"),(33,"7H"),(34,"8H"),(35,"9H"),(36,"10H"),(37,"11H"),(38,"12H"),(39,"13H"),
    (40,"1S"),(41,"2S"),(42,"3S"),(43,"4S"),(44,"5S"),(45,"6S"),(46,"7S"),(47,"8S"),(48,"9S"),(49,"10S"),(50,"11S"),(51,"12S"),(52,"13S")]

--Custom hand data type

data Hand = Hand {pokerNums :: [Int]
                , readableHand :: [String]
                , handVal :: Int
                , handValName :: String
                } deriving (Eq, Show, Read)

--Generic helper functions

occurencesUnique num list = [x| x<- unique list, x==num]
occurences num list = [x| x<- list, x==num]

compareFirstTwo x1 x2 = x2 - x1 ==1

has [] _ = False
has (x:xs) a
  | x == a    = True
  | otherwise = has xs a

unique [] = []
unique (x:xs)
  | has xs x  = unique xs
  | otherwise = x : unique xs

listToTup list = (list,list)

--Helper functions for working with poker hands
handNoSuit list  = [((x-1) `mod` 13)+1 | x<-list]

handToSuit list = [(x-1) `div` 13 | x<-list]

--getReadableHand hand = [show (((x-1) `mod` 13)+1) ++ ["C","D","H","S","S"] !! ((x-1) `div` 13) | x<- (sortBetter hand [])]
getReadableHand hand = [ snd(pokerDeck !! x) | x <- (sortBetter hand [])]

sortPair originalHand hand acc
    | hand ==[] = acc
    | otherwise = sortPair originalHand (tail hand) (injectPair (head hand) originalHand acc)

injectPair :: Int -> [Int] -> [Int] -> [Int]
injectPair x hand [] = [x] 
injectPair x hand (y:ys) = if (length (occurences x hand)) <= length((occurences y hand))
                            then y:x:ys
                            else x : (injectPair y hand ys)

getHandVal hand
    | (detectStraight (handNoSuit hand) == True)&& (detectFlush hand False 3 == True)=8
    | detectFlush hand False 3 == True = 5
    | detectStraight (handNoSuit hand)  == True = 4
    | otherwise = detectPairs (getPairs (handNoSuit hand) [])

separateHands list = ([snd x |x<-zip [0..9] list, (fst x)`mod`2==1 ]) : ([snd y| y<- zip [0..9] list, (fst y)`mod`2==0]) : []

getHandValName num = snd(pokerHands !! num)

makeHands list = (Hand {pokerNums=(sortBetter ((separateHands list) !! 0) []) 
, readableHand= getReadableHand (sortBetter((separateHands list) !! 0)[])
, handVal = getHandVal ((separateHands list) !! 0)
, handValName = getHandValName (getHandVal ((separateHands list) !! 0))}) : (Hand {pokerNums=(sortBetter ((separateHands list) !! 1) [])
, readableHand= getReadableHand (sortBetter((separateHands list) !! 1)[]), handVal = getHandVal ((separateHands list) !! 1)
, handValName = getHandValName (getHandVal ((separateHands list) !! 1))}) :[]

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

--Detection functions

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

getPairs (x:xs) acc
    | xs==[] = acc
    | otherwise = getPairs xs (acc++ (occurencesUnique x xs))

detectPairs list
    | (unique list == list) && (length list ==1) = 1
    | (unique list == list) && (length list ==2) = 2
    | (length list == 3) && length (unique list)==2 = 6
    | (length list == 3) && length (unique list)==1 = 7
    | otherwise = 0

--Comparison functions

compareHighCards tup1 tup2
    | head (handNoSuit(snd tup1)) == head (handNoSuit((snd tup2))) && length(snd tup1)==1 =[]
    | head (handNoSuit(snd tup1)) == head (handNoSuit(snd tup2)) = compareHighCards (fst tup1, (tail (snd tup1))) (fst tup2, (tail (snd tup2)))
    | (head (handNoSuit(snd tup1)) ==1) && (head (handNoSuit(snd tup2)))>1 = fst tup1
    | (head (handNoSuit(snd tup2))) ==1 && (head (handNoSuit(snd tup1)))>1 = fst tup2
    | head (handNoSuit(snd tup1)) > head (handNoSuit(snd tup2)) = fst tup1
    | head (handNoSuit(snd tup2)) > head (handNoSuit(snd tup1)) = fst tup2
--This function compares 2 Flushes in a Tie breaker
compareHighSuits tup1 tup2
    | (((head (handNoSuit(snd tup1)))`mod` 13) ==1) && (((head (handNoSuit(snd tup1)))`mod` 13)>1) = fst tup1
    | (((head (handNoSuit(snd tup2))) `mod` 13) ==1) && (((head (handNoSuit(snd tup2))) `mod` 13)>1) = fst tup2
    | head ((snd tup1)) == head (((snd tup2))) && length(snd tup1)==1 =[]
    | head ((snd tup1)) == head ((snd tup2)) = compareHighCards (fst tup1, (tail (snd tup1))) (fst tup2, (tail (snd tup2)))
    | head ((snd tup1)) > head ((snd tup2)) = fst tup1
    | head ((snd tup2)) > head ((snd tup1)) = fst tup2
--This function compares 2 straights in a Tie Breaker
compareStraights hand1 hand2
    | (((last hand1)-1)`mod`13)+1 > (((last hand2)-1)`mod`13)+1 = hand1
    | (((last hand2)-1)`mod`13)+1 > (((last hand1)-1)`mod`13)+1 = hand2
    | otherwise = []
--This function 
compareHands listOfHands
    | handVal ( listOfHands !! 0) >handVal (listOfHands !! 1) = readableHand (listOfHands !! 0)
    | handVal ( listOfHands !! 1) >handVal (listOfHands !! 0) = readableHand (listOfHands !! 1)
    | (has [1,2,3,6,7] (handVal ( listOfHands !! 1)))&&(compareHighCards(listToTup (sortPair (pokerNums(listOfHands !!0)) (pokerNums(listOfHands !!0)) [])) (listToTup (sortPair (pokerNums(listOfHands !!1)) (pokerNums(listOfHands !!1)) [])))/=[] = getReadableHand(compareHighCards(listToTup (sortPair (pokerNums(listOfHands !!0)) (pokerNums(listOfHands !!0)) [])) (listToTup (sortPair (pokerNums(listOfHands !!1)) (pokerNums(listOfHands !!1)) [])))
    | (has [4,8] (handVal ( listOfHands !! 1))) && (compareStraights (pokerNums(listOfHands !!0))  (pokerNums(listOfHands !!1)))/=[]= getReadableHand (compareStraights (pokerNums(listOfHands !!0))  (pokerNums(listOfHands !!1)))
    | (compareHighCards (listToTup ((pokerNums (listOfHands !! 0)))) (listToTup ((pokerNums (listOfHands !! 1))))) /= [] = getReadableHand(compareHighCards (listToTup ((pokerNums (listOfHands !! 0)))) (listToTup ((pokerNums (listOfHands !! 1)))))
    | otherwise = getReadableHand (compareHighSuits (listToTup (reverse(sortBetter (pokerNums (listOfHands !! 0)) []))) (listToTup (reverse(sortBetter (pokerNums (listOfHands !! 1)) []))))

--Main function
deal tenCards = compareHands(makeHands tenCards)