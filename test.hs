
--number from ascending digits starting from end
reverseOrdSuff num=reverseOrdSuffhelper num 0
 where
  reverseOrdSuffhelper num result
   |num<10 =result*10 + num
   |mod num 10 < mod (div num 10) 10 = reverseOrdSuffhelper (div num 10) (result*10+ (mod num 10))
   |otherwise = result*10 + mod num 10
   --summing unique ints from nested list
sumUnique :: [[Int]] -> Int
sumUnique xss = sum(concatMap (\lst -> [x| x<-lst, (countx x lst)<2]) xss)
 where countx x []=0
       countx x (y:ys) = if x==y then 1+countx x ys else countx x ys

--store querys
type Product = (String,Double) 
type StoreAvailability = [Product]
closestToAverage :: StoreAvailability -> String
closestToAverage xs= (head [x| (x,y)<-xs, abs(y-average)== minimum(map(\diff->abs(  snd diff - average)) xs)])
 where
  listPrices=map (\x->snd x) xs 
  average=(sum listPrices)/ (fromIntegral (length listPrices))

cheaperAlternative :: StoreAvailability -> Int
cheaperAlternative [] = 0
cheaperAlternative (y:ys)= if any (\x-> fst x==fst y && snd x/=snd y) ys then 1+ cheaperAlternative ys else cheaperAlternative ys
store2=[("bread",1),("cheese",2.5),("bread",1),("cheese",5),("cheese",2.3)]



--min distance between a list of points
minDistance :: [(Double,Double,Double)] -> Double
minDistance xs=minimum(minDistancehelper xs)
minDistancehelper [x]=[]
minDistancehelper (x:xs)=(minimum(map (\point2-> distance point2 x) xs)):(minDistancehelper xs)
 where
  distance (x,y,z) (x1,y1,z1) = (x-x1)*(x-x1)+(y-y1)*(y-y1)+(z-z1)*(z-z1)
--to fix maximize 
--maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a)
--maximize fs=(\x-> head [f| f<-fs, f x==(maximum(map (\func->func x) fs))])
--fn = maximize [(\x -> x*x*x),(\x -> x+1)]

--check if f.g = id && g.f=id in interval
inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun f g a b =all (==True) (map (\num ->  ((g . f) num ==num) && ((f . g) num==num))[a..b])


--binary tree tasks
data BTree a= Empty | Node a (BTree a) (BTree a)
 deriving Show

inorder::BTree a -> [a]
inorder Empty = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right 

tree1 = Node 5

            (Node 1 Empty Empty)

            (Node 2 Empty (Node 10 Empty Empty))
mirrorBst :: BTree а -> BTree а
mirrorBst (Node x left right)=(Node x (mirrorBst right) (mirrorBst left))
mirrorBst Empty = Empty

getLevels :: BTree a -> [(a,Int)]
getLevels tree = getLevelshelper tree 0
getLevelshelper (Node x left right) level = [(x, level)]++(getLevelshelper left (level+1))++(getLevelshelper right (level+1))
getLevelshelper Empty _=[]