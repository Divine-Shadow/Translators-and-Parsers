import Text.ParserCombinators.Parsec
import Debug.Trace
data Exp = IntExp Integer
         | SymExp String
         | SExp [Exp]
         deriving (Show)

data Val = IntVal Integer
         | SymVal String
         | PrimVal ([Val] -> Val)
         | Nil
         | DefVal String Val
         | Closure [String] Exp  [(String,Val)] 
         | Existential Bool
         | EpicFail String
      
instance Ord Val where
  compare (IntVal a) (IntVal b) = compare a b
  compare (SymVal a) (SymVal b) = compare a b

instance Eq Val where
  (==) (IntVal a) (IntVal b) = (==) a b
  (==) (SymVal a) (SymVal b) = (==) a b

instance Show Val where
  show (IntVal i) = show i
  show (SymVal s) = s
  show Nil = "nil"
  show (DefVal x xs) = show x  
  show (Closure _ _ _ ) = "*Closure*"
  show (Existential True) = "t"
  show (Existential False) = "nil"
  show (EpicFail s) = s
showRaw (SymExp p) = p

run x = parseTest x

-- Lexicals
number = ['0'..'9']
adigit:: Parser Char
adigit = oneOf number
digits = many1 adigit

symbol:: Parser Char
symbolList = "!#$%&|*+-/:<=>?@^_~`\"-\'"
symbol = oneOf symbolList
symbols = many1 symbol

-- Grammaticals
anInt::Parser Exp
anInt = do d <- digits
           spaces
           return $ IntExp (read d)

aSym::Parser Exp
aSym = do car <- symbol <|> letter
          cdr <- many (letter <|> digit <|> symbol)
          spaces      
          let theSym = car:cdr
          return $ SymExp theSym
               
            
aform::Parser Exp
aform = do char '('
           k <- many (aform <|> aSym <|> anInt)
           char ')'
           spaces
           return $ SExp k 
anExp = aSym <|> anInt <|> aform
-- Evaluator

eval :: Exp -> [(String,Val)] -> Val
eval (IntExp i) env = IntVal i
eval (SExp []) env = Nil
eval (SExp expression) env = case expression of
                              ([(SymExp "def"),thename,thevalue]) -> DefVal (showRaw thename) (eval thevalue env)
                              ([(SymExp "define"),thename,(SExp theargs),thevalue]) -> DefVal (showRaw thename) u where u = Closure (map showRaw theargs) thevalue ((showRaw thename, u):env)
                              ([(SymExp "lambda"),(SExp theargs),thevalue]) -> Closure (map showRaw theargs) thevalue env
                              ([(SymExp "quote"), value]) -> (quotify value)
                              ((SymExp "<"):x) -> megaCompareLift (<) (myMap eval env x)
                              ((SymExp ">"):x) -> megaCompareLift (>) (myMap eval env x)
                              ((SymExp "and"):x) -> megaboolLift (&&) (myMap eval env x)
                              ((SymExp "or"):x) -> megaboolLift (||) (myMap eval env x)
                              ((SymExp "not"):x) -> megaboolLift (mynot) (myMap eval env x)
                              ((SymExp "eq?"):x) -> megaCompareLift (==) (myMap eval env x)                 
                              ([(SymExp "cond"),x]) -> condEval x env
                              (x:xs) ->  case (eval x env) of
                                          (PrimVal v) -> v (myMap eval env xs)
                                          (Closure args expr environment) -> eval expr ((zip args (myMap eval env xs)) ++ environment)
                                          otherwise -> EpicFail (show expression) 
                              otherwise -> EpicFail (show expression)
                                          
eval (SymExp (('\''):value)) env = (SymVal value)
eval (SymExp s) env = specialLookup s env



specialLookup::String -> [(String, Val)] -> Val
specialLookup key = foldr (\(k,v) pastResult -> if key == k then v else pastResult) (SymVal "command lexed but not found")

myMap f a [] = []
myMap f a (x:xs) = (f x a):(myMap f a xs)

condEval (SExp (x:xs:ss)) env = ifff (getBool (eval x env)) (eval xs env) (condEval (SExp ss) env)
condEval (SExp []) env = Nil
  



quotify::Exp->Val
quotify (SymExp p) = (SymVal p)


repl defs =
  do putStr "> "
     l <- getLine
     
     case parse anExp "Expression" l  of
          Right exp -> let m = eval exp defs
                       in 
                          do
                           putStr $ show m 
                           putStrLn ""
                           case m of
                             (DefVal x xs) -> repl ((x,xs):defs)
                             _ -> repl defs
          Left pe   -> putStr (show pe)
     putStrLn ""
     repl defs
--
getBool::Val -> Bool
getBool (Existential a) = a
getBool (SymVal "t") = True
getBool Nil = False
getBool x = trace (show x) True


boolLift f a b = (f (getBool a) (getBool b))

megaboolLift f [x] = Existential (myBoolFold (boolLift f) [x,(Existential True)])
megaboolLift f x = Existential (myBoolFold (boolLift f) x)
mynot a _ = not a

 

liftIntOp::(Integer->Integer->Integer) -> ([Val] -> Val)
liftIntOp f list = IntVal (auxliftIntOp f list)
auxliftIntOp f [a,b]= f (maths a) (maths b)
auxliftIntOp f (x:xs) = f (maths x) (auxliftIntOp f xs) 
maths (IntVal i) = i

megaCompareLift f xs = Existential (myBoolFold f xs)

--compareLift::Ord x => (x -> x -> Bool) -> Val -> Val -> Bool
--compareLift f (IntVal a) (IntVal b) = (f a b )
--compareLift f (SymVal a) (SymVal b) = (f a b)

myBoolFold operator (a:b:xs) = ifff (operator a b) (myBoolFold operator (b:xs)) False
myBoolFold operator [x] = True


ifff True a _ = a
ifff False _ b = b
--Initial Environments
basic = [("+" , PrimVal (liftIntOp (+)))]
level1 = [("+" , PrimVal (liftIntOp (+))),("t", Existential True) , ("nil", Existential False)]
level2 = [("+" , PrimVal (liftIntOp (+))),("-" , PrimVal (liftIntOp (-))),("*" , PrimVal (liftIntOp (*))),("t", Existential True) , ("nil", Existential False)]