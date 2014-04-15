import Text.ParserCombinators.Parsec

data Exp = IntExp Integer
         | SymExp String
         | SExp [Exp]
         | DefVal [Exp]
         deriving (Show)

data Val = IntVal Integer
         | SymVal String
         | PrimVal ([Val] -> Val)
         | Nil
         | Closure [String] Exp  [(String,Val)] 
         | FuncVal [Exp]


run x = parseTest x

-- Lexicals
--adigit::Text.Parsec.Prim.ParsecTString u Data.Functor.Identity.Identity Char
number = ['0'..'9']
adigit:: Parser Char
adigit = oneOf number
digits = many1 adigit

symbol:: Parser Char
symbolList = "!#$%&|*+-/:<=>?@^_~`\"-"
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
eval (SExp (s:sx)) env = case (eval s env) of 
                         (PrimVal v) -> v (myMap eval env sx)
                         (SymVal "define") ->  eval (DefVal sx) env
eval (DefVal (x:xs)) env = repl (show x, xs):env
eval (SymExp s) env = specialLookup s env

specialLookup::String -> [(String, Val)] -> Val
specialLookup key = foldr (\(k,v) pastResult -> if key == k then v else pastResult) (SymVal "utter failure")

myMap f a [] = []
myMap f a (x:xs) = (f x a):(myMap f a xs)

--primCheck (PrimVal v) args = v args
--primCheck (SymExp "define") (x:xs) = DefVal xs x
--primCheck _ arg = SymVal "epic fail"
-- Printer

instance Show Val where
  show (IntVal i) = show i
  show (SymVal s) = show s
  show Nil = "nil"
  


repl defs =
  do putStr "> "
     l <- getLine
     case parse anExp "Expression" l of
       Right exp -> putStr (show (eval exp defs))
       Left pe   -> putStr (show pe)
     putStrLn ""
     repl defs
--

--liftIntOp f a  =  IntVal (foldr f a )
liftIntOp::(Integer->Integer->Integer) -> ([Val] -> Val)
liftIntOp f list = IntVal (auxliftIntOp f list)
auxliftIntOp f [a,b]= f (maths a) (maths b)
auxliftIntOp f (x:xs) = f (maths x) (auxliftIntOp f xs) 
maths (IntVal i) = i
--Initial Environments
basic = [("+" , PrimVal (liftIntOp (+)))]