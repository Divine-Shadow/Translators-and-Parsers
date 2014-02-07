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