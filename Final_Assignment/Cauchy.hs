module Cauchy where
data CauchyList = CauchyList {
    p :: Int
    , content :: [Int]
}
modList num list = [x `mod` num|x<- list]
add a b = a+b
sub a b = a-b
multi list1 list2 acc
    | list1==[] && list2==[] = acc
    | otherwise = multi (tail list1) (init list2) (acc+((head list1)*(last list2)))

zipWithZero fn (x:xs) (y:ys) = (fn x y) : zipWithZero fn xs ys
zipWithZero fn []     ys     = zipWith (fn) (repeat 0) ys
zipWithZero fn xs     []     = zipWith (fn) xs (repeat 0)

addZeroes len list = zipWithZero add list (take len (repeat 0))

multiplyCauchy list1 list2 result
    | list1==[] && list2==[] = reverse result
    | otherwise = multiplyCauchy (init list1) (init list2) (result++[(multi list1 list2 0)])

mCauchy list1 list2 = (take ((length list1) + (length list2) -1) (repeat 0))

instance Eq CauchyList where
    (CauchyList p1 list1) == (CauchyList p2 list2) = (p1==p2 && list1==list2)

instance Num CauchyList where

    (CauchyList p1 list1) + (CauchyList p2 list2) = (CauchyList p1 (modList p1 (zipWithZero add list1 list2)))
    (CauchyList p1 list1) - (CauchyList p2 list2) = (CauchyList p1 (modList p1 (zipWithZero sub list1 list2)))
    abs(CauchyList p1 list1) = (CauchyList p1 list1)
    (CauchyList p1 list1) * (CauchyList p2 list2) = (CauchyList p1 (modList p1 (multiplyCauchy (addZeroes ((length list1) + (length list2) -1) list1) (addZeroes ((length list1) + (length list2) -1) list2) [])))
    fromInteger i = CauchyList (fromIntegral i) [(fromIntegral i)]
    signum (CauchyList p1 list1) = CauchyList (signum p1) list1


instance Show CauchyList where
    show (CauchyList p list) = "P: "++(show p)++"\nLength: "++(show (length list))++"\nContent: "++(show list)