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
   | Label [String]
 

instance Show Entry where
  show (Prim f)    = "Prim"
  show (Def s)     = show s
  show (Num i)     = show i
  show (Unknown s) = "Unknown: " ++ s
  show (Label s) = show s

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
 
-- My functions part I   
      
   -- My stack manipulators
drop2 (x:xs) = xs
drop2 _ = []

eUnderflow = error "Value stack underflow!"

dup (x:xs) = (x:x:xs)
dup _ =  eUnderflow
  
swap (x:y:xs) = (y:x:xs)
swap _ = eUnderflow

rot (x:f:d:xs) = (d:x:f:xs)
rot _ = eUnderflow

  -- my comparison operators
lessThan (a:b:c) = (ifff (b < a) (-1) 0):c
greaterThan (a:b:c) = (ifff (b > a) (-1) 0):c
equalto  (a:b:c) = (ifff (a == b) (-1) 0):c

  -- my own if statement
ifff a t f  
  | a==True = t
  | otherwise = f

-- Initial Dictionary

  -- A more functional approach to initializing the values
initDict (q:qs) (a:as) dict =  dinsert q (Prim $ a) (initDict qs as dict) 
initDict [] _ dict = dict

  -- And said initialization
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
    --
    Unknown "if" -> do 
                     putStrLn $ show $ whereIf xs 1
                     putStrLn $ show $ whereElse xs 1
                     putStrLn $ show $ whereThen xs 1           
                     eval whichBranch ((tail istack), remainingWords:cstack, dict)
                     
    Unknown "then" -> do  
                       eval (head cstack) (istack, (tail cstack), dict)                
    --
    Unknown "begin" -> eval loopContents (istack, loopContents:((drop againloc xs):cstack), dict) 
    Unknown "again" -> eval (head cstack) (istack, cstack, dict)
    --
    Unknown "Clear" -> eval [] ([], [], dict) 
    --
    Unknown "goto" ->  eval  (labelRetriever (dlookup (head (head destinationStack)) dict)) (istack, (tail destinationStack), dict)
    Unknown "label" -> eval (tail xs) (istack, ([(head xs)]:cstack), dinsert (head xs) (Label (tail xs)) dict) 
    
    Label p -> eval (tail words) (istack, cstack, dict)
      
    Def p  ->  eval  p (istack, xs:cstack, dict)
    
    Unknown p -> do  
                putStrLn $ head words 
                eval (head cstack) (istack, (tail cstack), dict)  
  
  where xs = tail words
        whichBranch = ifff ((head istack) == -1) (trueBranch xs) (falseBranch xs)
        remainingWords =  (drop (whereThen xs 1) xs)
        loopContents = take againloc xs
        againloc = whereAgain xs 1
        destinationStack = labelFinder cstack (head xs)
        


-- My functions part II
        

  
        
        
        
labelFinder (a:as) name
  | a == [name] = (a:as)
  | otherwise = labelFinder as name
labelFinder [] name =  []
        
labelRetriever (Label p) = p
 -- Tokens

  -- finds the  token 
  -- if a is 0 then the "take" value of the output will exclude the token
  -- if a is 1 the "drop" value of the return will drop the token statement
tokenFinder (d:ds) a token
  | d == token = a
  | otherwise = tokenFinder ds (a+1) token
tokenFinder [] _  _ = -1

  -- frequently used tokens
whereAgain m a = tokenFinder m a "again"
whereThen m a = metaTokenFinder m a "then" 0
whereElse m a = metaTokenFinder m a "else" 0
whereIf m a = metaTokenFinder m a "if" 0
findSplit m a = tokenFinder m a ";"
--
metaTokenFinder (d:ds) loc token nestlevel
  | ((d == token)&&(nestlevel==0)) = loc 
  | (d == "if") = metaTokenFinder ds loc token (nestlevel + 1)
  | (d == "then") = metaTokenFinder ds loc token (nestlevel - 1)
  | otherwise = metaTokenFinder ds (loc + 1) token nestlevel
metaTokenFinder [] _  token  _ = -1

 -- True False path identifiers
  -- returns the true branch of an expression
trueBranch ds
 | whereElse ds 0== -1 = take (whereThen ds 1) ds
 | otherwise = take (whereElse ds 0) ds
               
  -- retruns the false branch if it exists and nil if it doesn't
falseBranch ds
  | whereElse ds 0== -1 = []
  | otherwise = take (whereThen newString 1) newString
  where newString = drop (whereElse ds 1) ds                     
                
 
-- prints the stack, used in .S
printstack (b:bs) = printstack bs ++ show b ++ " "
printstack _ = ""


-- The repl
repl :: ForthState -> IO ForthState
repl state =
  do putStr "> " ;
     input <- getLine
     nustate <- eval (words input) state
     repl nustate

main = do
  putStrLn "Welcome to your forth interpreter!"
  repl (initialIStack, initialCStack, dictionary)
