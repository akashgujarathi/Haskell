slice:: Int->Int->[Int]->[Int]
slice x y z=slicee x y z 1


slicee::Int->Int->[Int]->Int->[Int]
slicee a b [] c =[] 
slicee a b (c:cs) d
 | a ==d && d<=b =c:slicee (a+1) b cs (d+1) 
 | d<b =slicee a b cs (d+1)
 | otherwise =[]


--------------------------------------------------------------------



delete::Int->[Int]->[Int]

delete x y= deletee x y 1

deletee::Int->[Int]->Int->[Int]
deletee a [] c =[]
deletee a (b:bs) c
 | c `mod` a ==0 =deletee a bs (c+1)
 | c `mod` a /=0 =b:deletee a bs (c+1)


----------------------------------------------------------------------
sortlist :: [[Int]] -> [[Int]]
sortlist [] = []
sortlist (x:xs)= sortlist [y | y<-xs, length y<=length x] ++ [x] ++ sortlist [y | y<- xs, length y>length x]

-----------------------------------------------------------------------
flatten::[[Int]]->[Int]
flatten []=[]
flatten [[]]=[]
flatten ((x:xs):y)=[x] ++ xs ++ flatten y

-----------------------------------------------------------------------
