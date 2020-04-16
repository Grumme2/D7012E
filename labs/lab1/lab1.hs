-- Anton Grahn

getSubListsOuterLoop :: ([Int],Int,Int) -> [([Int],Int,Int,Int)]
getSubListsOuterLoop (a,i,j) 
    |length a == 1 = [(a,i,j,foldr (+) 0 a)]
    |otherwise = getSubListsInnerLoop(a,i,j) ++ getSubListsOuterLoop(tail a,i+1,j)

getSubListsInnerLoop :: ([Int],Int,Int) -> [([Int],Int,Int,Int)]
getSubListsInnerLoop (b,i,j) 
    |1 == length b = [(b,i,j,foldr (+) 0 b)]
    |otherwise = [(b,i,j,foldr (+) 0 b)] ++ getSubListsInnerLoop(init b, i, j-1) 

getSubLists :: ([Int]) -> [([Int],Int,Int,Int)]
getSubLists (c)
    |0 == length c = [([0],0,0,0)]
    |otherwise = mergesort(getSubListsOuterLoop(c,0,(length c)-1))

mergesort :: ([([Int], Int, Int, Int)]) ->  [([Int], Int, Int, Int)] 
mergesort (list)
    |1 == length list = list
    |2 == length list = mergsort_split(splitAt 1 list)
    |otherwise = mergsort_split(splitAt (length list `div` 2) list)


mergsort_split ::  ([([Int], Int, Int, Int)],  [([Int], Int, Int, Int)]) ->  [([Int], Int, Int, Int)] 
mergsort_split (left, right) = mergsort_merge (mergesort(left), mergesort(right))

mergsort_merge ::  ([([Int], Int, Int, Int)],  [([Int], Int, Int, Int)]) ->  [([Int], Int, Int, Int)] 
mergsort_merge (left,right)
    |0 == length left = right
    |0 == length right = left 
    |ntuplevalue(head left,3) <= ntuplevalue(head right,3 ) = [head(left)] ++ mergsort_merge(tail left, right)
    |otherwise = [head right] ++ mergsort_merge(left, tail right)

smallestk :: ([Int],Int) -> IO()
smallestk (list, k) 
    |length list == 0 = putStr("List is empty!!\n")
    |otherwise =  putStr("\n\n\nsize\ti\tj\tsublist\n"++printk(getSubLists(list),k))

printk ::  ([([Int], Int, Int, Int)],Int) ->  String
printk (i: list, k)
    |k == 0 = "\n"
    |otherwise = show(ntuplevalue(i,0)) ++ "\t" ++ show(ntuplevalue(i,1)) ++ "\t"++ show(ntuplevalue(i,2)) ++  "\t" ++ show(getlistintuple(i)) ++ "\n" ++ printk(list,k-1)   
--  
-- 
ntuplevalue :: (([Int], Int, Int, Int),Int) -> Int
ntuplevalue ((list, i, j, size),n)
    -- |n==0 = list
    |n==1 = i
    |n==2 = j
    |otherwise = size

getlistintuple ::  ([Int], Int, Int, Int) -> [Int]
getlistintuple (list,i,j,size) = list

