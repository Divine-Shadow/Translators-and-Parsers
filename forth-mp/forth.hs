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

wrap2 f (x:y:xs) = (f y x):xs
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

eUnderflow = error "Value stack underflow!"

dup (x:xs) = (x:x:xs)
dup _ =  eUnderflow
  
swap (x:y:xs) = (y:x:xs)
swap _ = eUnderflow

rot (x:f:d:xs) = (d:x:f:xs)
rot _ = eUnderflow

  -- my own if statement
ifff a t f  
  | a==True = t
  | otherwise = f
                

  -- my comparison operators
lessThan (a:b:c) = (ifff (b < a) (-1) 0):c
greaterThan (a:b:c) = (ifff (b > a) (-1) 0):c
equalto  (a:b:c) = (ifff (a == b) (-1) 0):c



-- Initial Dictionary

{-dictionary0 = dinsert "+" (Prim $ wrap2 (+)) H.empty
dictionary1 = dinsert "drop" (Prim $ drop2) dictionary0
dictionary2 = dinsert "rot" (Prim $ rot ) dictionary1
dictionary3= dinsert "swap" (Prim $ swap ) dictionary2
dictionary4= dinsert "dup" (Prim $ dup ) dictionary3
dictionary5 = dinsert "-" (Prim $ wrap2 (-)) dictionary4
dictionary6 = dinsert "<" (Prim $ lessThan) dictionary5
dictionary7 = dinsert ">" (Prim $ greaterThan) dictionary6
dictionary8 = dinsert "=" (Prim $ equalto) dictionary7
dictionary9 = dinsert "*" (Prim $ wrap2 (*)) dictionary8
dictionary  = dinsert "/" (Prim $ wrap2 div) dictionary9 -}
 -- A more functional approach to initializing the values
initDict (q:qs) (a:as) dict =  dinsert q (Prim $ a) (initDict qs as dict) 
initDict [] _ dict = dict

dictionary = initDict [ "+", "drop", "rot", "swap" , "dup" , "-",  "<",  ">",  "=" ,  "*", "/"] [ wrap2 (+), drop2, rot, swap, dup, wrap2 (-), lessThan, greaterThan, equalto, wrap2 (*), wrap2 div] H.empty 

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
    Unknown ":"  -> eval (drop (findSplit xs 1) xs) (istack, cstack, dinsert (head xs) (Def (tail (take (findSplit xs 0) xs))) dict)
    Unknown "if" -> eval whichBranch ((tail istack), remainingWords:cstack, dict)
                    
    Unknown "begin" -> eval loopContents (istack, loopContents:((drop againloc xs):cstack), dict) 
    Unknown "again" -> eval (head cstack) (istack, cstack, dict)
    Unknown "exit" -> eval (head (tail cstack)) (istack, (tail (tail cstack)), dict)
    
    Def p  ->  eval  p (istack, xs:cstack, dict)
  
  where xs = tail words
        whichBranch = ifff ((head istack) == -1) (trueBranch xs) (falseBranch xs)
        remainingWords =  (drop (whereThen xs 1) xs)
        loopContents = take againloc xs
        againloc = whereAgain xs 1

-- finds the else token, if a is 0 then the take value of the output will exclude the else statement, if a is 1 the drop value of the return will drop the else statement
whereElse (d:ds) a
  | d == "else" = a
  | otherwise = whereElse ds (a+1)
whereElse [] _ = -1

whereAgain (d:ds) a
  | d == "again" = a
  | otherwise = whereAgain ds (a+1)
whereAgain [] _ = -1

-- finds the then token, if a is 0 then the take value of the output will exclude the then statement, if a is 1 the drop value of the return will drop the Then statement
whereThen (d:ds) a
  | d == "then" = a
  | otherwise = whereThen ds (a+1)
                
-- returns the true branch of an expression
trueBranch ds
 | whereElse ds 0== -1 = take (whereThen ds 0) ds
 | otherwise = take (whereElse ds 0) ds
               
-- retruns the false branch if it exists and nil if it doesn't
falseBranch ds
  | whereElse ds 0== -1 = []
  | otherwise = take (whereThen newString 0) newString
  where newString = drop (whereElse ds 1) ds                     
                
                
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
