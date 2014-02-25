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
drop (x:xs) = xs
drop _ = []

dup (x:xs) = (x:x:xs)
dup _ = error "Value stack underflow!"
  
swap (x:y:xs) = (y:x:xs)
swap _ = error "Value stack underflow!"

rot (x:f:d:xs) = (d:x:f:xs)
rot _ = error "Value stack underflow!"

-- my arithmatic


-- Initial Dictionary

dictionary1 = dinsert "+" (Prim $ wrap2 (+)) H.empty
dictionary2 = dinsert "rot" (Prim $ rot ) dictionary1
dictionary3= dinsert "swap" (Prim $ swap ) dictionary2
dictionary4= dinsert "dup" (Prim $ dup ) dictionary3
dictionary5 = dinsert "-" (Prim $ wrap2 (-)) dictionary4
dictionary  = dinsert "/" (Prim $ wrap2 (/)) dictionary5

-- The Evaluator

eval :: [String] -> ForthState -> IO ForthState
eval []    (istack, [],     dict) = return (istack, [], dict)
eval words (istack, cstack, dict) =
  case dlookup (head words) dict of
    Num i        -> eval xs (i:istack, cstack, dict)
    Prim f       -> eval xs (f istack, cstack, dict)
    Unknown "."  -> do { putStrLn $ show (head istack);
                             eval xs (tail istack, cstack, dict) }
  where xs = tail words

repl :: ForthState -> IO ForthState
repl state =
  do putStr "> " ;
     input <- getLine
     nustate <- eval (words input) state
     repl nustate

main = do
  putStrLn "Welcome to your forth interpreter!"
  repl (initialIStack, initialCStack, dictionary)
