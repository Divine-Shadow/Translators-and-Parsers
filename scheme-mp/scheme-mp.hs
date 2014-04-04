import Text.ParserCombinators.Parsec

data Exp = IntExp Integer
         | SymExp String
         | SExp
         deriving (Show)

data Val = IntVal Integer
         | SymVal String

run x = parseTest x

-- Lexicals
--adigit::Text.Parsec.Prim.ParsecTString u Data.Functor.Identity.Identity Char
adigit = oneOf ['0'..'9']
digits = many1 adigit
symbol:: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~`\"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
symbols = many1 symbol

-- Grammaticals

anInt = do d <- digits
           spaces
           return $ IntExp (read d)
--aSym = do s <- symbols 
  --        spaces
    --      return $ SymExp (read s)
anAtom = anInt

anExp = anAtom

-- Evaluator

eval :: Exp -> [(String,Val)] -> Val
eval (IntExp i) env = IntVal i
eval (SymExp s) env = SymVal s
-- Printer

instance Show Val where
  show (IntVal i) = show i

repl defs =
  do putStr "> "
     l <- getLine
     case parse anExp "Expression" l of
       Right exp -> putStr (show (eval exp defs))
       Left pe   -> putStr (show pe)
     putStrLn ""
     repl defs
--

liftIntOp f a = foldr f a
