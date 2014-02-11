module Mp1 where

fac n = if n==1 then 1 else n * fac (n-1) 

gcd a b | b == 0 = a
      	| a < b = Mp1.gcd b a
	| otherwise = Mp1.gcd b  (a `mod`  b )

calc x = calcs x 0
calcs x c | x == 1 = c
       	 | even x = calcs (quot x 2) (c + 1)  
	 | otherwise = calcs (3*x+1) (c+1)

hd (x:xs) = x
tl (x:xs) = xs


mytake n (x:xs) | n==0 = []
       	 	| otherwise = x:(mytake (n-1) xs)

mydrop n (x:xs) | n==0 = (x:xs)
       	 	| otherwise =(mydrop (n-1) xs)


rev xxx= revAux xxx []
revAux [] y=y
revAux (x:xs) y = revAux xs (x:y)

app xx yy | xx/=[] = ((hd xx):(app (tl xx) yy))
          | otherwise = yy 

inclist x = map inc x
inc x = x+1  

doublelist x = map doubler x
doubler x = 2*x

sumlist x = foldr plus 0 x
plus a b = a+b

prodlist x = foldr prod 1 x
prod a b = a*b

zip :: [a] -> [b] -> [(a,b)]
zip (a:as) (b:bs) = (a,b) : Mp1.zip as bs
zip _      _      = []

addpairs (x:xs) (y:ys) = (x+y): Mp1.addpairs xs ys
addpairs _      _      = []

element x (y:yy) = ifff (x==y) True (element x yy)
element _ _      = False
ifff True x _ =x
ifff False _ y =y

add x [] = [x]
add x (y:ys) | x<y = x:y:ys
      	     | x==y = x:ys
	     |otherwise = y:add x ys

del x [] = []
del x (y:ys)= ifff (y==x) ys (y:(del x ys))

union (xx:xs) (yy:ys) | xx < yy = xx:(union xs (yy:ys))
      	      	      | xx == yy = xx:(union xs ys)
		      | xx > yy = yy:(union (xx:xs) ys)
union a []=a
union [] b = b 

intersect (xx:xs) (yy:ys) | xx<yy = intersect xs (yy:ys)
	  	  	  | xx==yy = xx:(intersect xs ys)
			  | otherwise= intersect (xx:xs) ys
intersect a [] = a
intersect [] b = b

powerset []=[[]]
powerset (x:xs)= hugeList ++ map (x:) hugeList
	 	 where hugeList= powerset xs

ones = 1:ones
nats = 1:map inc nats

