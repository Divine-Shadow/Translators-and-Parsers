import Data.HashMap.Strict as H

-- Initial types

type ForthState = (IStack, CStack, Dictionary)

type IStack = [Integer]
initialIStack = []

type CStack = [[String]]
initialCStack = []

-- Type for the symbol dictionary

type Dictionary = H.HashMap String [Entry]

data Entry =
     Prim ([Integer] -> [Integer])
   | Def [String]
   | Num Integer
   | Unknown String

instance Show Entry where
  show (Prim f)    = "Prim"
  show (Def s)     = show s
  show (Num i)     = show i
  show (Unknown s) = "Unknown: " ++ s

-- Dictionary helpers

wrap2 f (x:y:xs) = (f x y):xs
wrap2 f _ = error "Value stack underflow!"



dlookup :: String -> Dictionary -> Entry
dlookup word dict =
  case H.lookup word dict of
    Nothing -> case reads word of
                 [(i,"")] -> Num i
                 _        -> Unknown word
    Just x  -> head x

dinsert :: String -> Entry -> Dictionary -> Dictionary
dinsert key val dict =
   case H.lookup key dict of
      Nothing -> H.insert key [val] dict
      Just x  -> H.insert key (val:x) dict
 -- my functions     
      -- my manipulators
drop2 (x:xs) = xs
drop2 _ = []

dup (x:xs) = (x:x:xs)
dup _ = error "Value stack underflow!"
  
swap (x:y:xs) = (y:x:xs)
swap _ = error "Value stack underflow!"

rot (x:f:d:xs) = (d:x:f:xs)
rot _ = error "Value stack underflow!"

ifff a t f  
  | a==True = t
  | otherwise = f
                

lessThan (a:b:c) = (ifff (a < b) (-1) 0):c
greaterThan (a:b:c) = (ifff (a < b) 0 (-1)):c

equalto  (a:b:c) = (ifff (a == b) (-1) 0):c
-- my arithmatic


-- Initial Dictionary

dictionary0 = dinsert "+" (Prim $ wrap2 (+)) H.empty
dictionary1 = dinsert "drop" (Prim $ drop2) dictionary0
dictionary2 = dinsert "rot" (Prim $ rot ) dictionary1
dictionary3= dinsert "swap" (Prim $ swap ) dictionary2
dictionary4= dinsert "dup" (Prim $ dup ) dictionary3
dictionary5 = dinsert "-" (Prim $ wrap2 (-)) dictionary4
dictionary6 = dinsert "<" (Prim $ lessThan) dictionary5
dictionary7 = dinsert ">" (Prim $ greaterThan) dictionary6
dictionary8 = dinsert "=" (Prim $ equalto) dictionary7
dictionary9 = dinsert "*" (Prim $ wrap2 (*)) dictionary8
dictionary  = dinsert "/" (Prim $ wrap2 div) dictionary9

-- The Evaluator

eval :: [String] -> ForthState -> IO ForthState
eval []    (istack, [],     dict) = return (istack, [], dict)
eval [] (istack, (h:hs) , dict) = eval h (istack, hs , dict)
eval words (istack, cstack, dict) =
  case dlookup (head words) dict of
    Num i        -> eval xs (i:istack, cstack, dict)
    Prim f       -> eval xs (f istack, cstack, dict)
    Unknown "."  -> do { putStrLn $ show (head istack);
                             eval xs (tail istack, cstack, dict) }
    Unknown ".S" -> do { putStrLn $ printstack istack;
                             eval xs (istack, cstack, dict)}
    Unknown ":"  -> do { putStrLn "recieved command"; 
                         eval (drop (findSplit xs 1) xs) (istack, cstack, dinsert (head xs) (Def (tail (take (findSplit xs 0) xs))) dict)}
                    
    Def p        -> do { putStrLn "def starting"; eval  p (istack, xs: cstack, dict)}
    Unknown _  -> do {putStrLn (show words); eval xs (istack, cstack, dict)}
  where xs = tail words

findSplit (c:cs) a
  | c == ";" = a
  | otherwise = findSplit cs (a + 1)

printstack (b:bs) = printstack bs ++ show b ++ " "
printstack _ = ""

repl :: ForthState -> IO ForthState
repl state =
  do putStr "> " ;
     input <- getLine
     nustate <- eval (words input) state
     repl nustate

main = do
  putStrLn "Welcome to your forth interpreter!"
  repl (initialIStack, initialCStack, dictionary)
