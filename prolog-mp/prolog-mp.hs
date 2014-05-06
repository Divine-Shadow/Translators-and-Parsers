import qualified Data.List as L
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as S

import Data.Hashable

import Text.ParserCombinators.Parsec

data Pattern = Var String
             | GVar Int
             | Obj String
             | Funct String [Pattern]
             | Prim String
     deriving Eq

instance Show Pattern where
  show (Var x) = x
  show (Obj x) = x
  show (Funct x xx) = x ++ "(" ++ (concat (L.intersperse "," (map show xx))) ++ ")"
  show (Prim s) = s
  show (GVar i) = "_G" ++ show i

instance Hashable Pattern where
  hashWithSalt s p = hashWithSalt s (show p)

data Bindings = Fail
              | NoBindings
              | Bindings (H.HashMap String Pattern)
     deriving Show

data Command = Implies Pattern [Pattern]
             | Query [Pattern]
     deriving Show

extendBindings k v Fail = Fail
extendBindings k v NoBindings = Bindings $ H.singleton k v
extendBindings k v (Bindings ht) =
   Bindings $ H.insert k v ht

lookupBindings p (Bindings ht) =
   let Just v = H.lookup p ht
    in v

hasBinding p (Bindings ht) =
   case H.lookup p ht of
     Just _  -> True
     Nothing -> False
hasBinding p _ = False

-- Database

type Database = H.HashMap String [[Pattern]]

addClause :: [Pattern] -> Database -> Database
addClause pattern@((Funct f xx):xs) database =
  let oldVal = H.lookupDefault [] f database
   in H.insert f (pattern:oldVal) database

lookupClause :: String -> Database -> [[Pattern]]
lookupClause p database = H.lookupDefault [] p database

-- show $ Funct "f" [Obj "x",Obj "y"]
-- => "f(x,y)"

-- Parser is included.
-- You're welcome.

run x = parseTest x

symbol s = do string s
              spaces
              return s

var = do v1 <- upper
         v2 <- many alphaNum
         spaces
         return $ Var (v1:v2)

obj = do v1 <- lower
         v2 <- many alphaNum
         o <- option (Obj (v1:v2))
                     (do symbol "("
                         args <- sepBy unit (symbol ",")
                         symbol ")"
                         return $ Funct (v1:v2) args)
         spaces
         return o

unit =    var
      <|> obj


assertion = do u <- unit
               symbol "."
               return u

rule = do u <- unit
          o <- option (Implies u [])
                      (do spaces
                          symbol ":-"
                          subgoals <- sepBy unit (symbol ",")
                          return $ Implies u subgoals)
          symbol "."
          return o

query = do symbol "?"
           subgoals <- sepBy unit (symbol ",")
           return $ Query subgoals

parseProlog = rule <|> query

-- Utilities I used.  You don't have to use them if you don't want them.

-- Cond, for those of you who know Lisp.  Haskell's lazy evaluation
-- makes this safe.

cond :: [(Bool,a)] -> a
cond [] = error "No conditions matched."
cond ((c,r):cc) =
   if c then r else cond cc

-- example:
-- signum x = cond [(x > 0     ,  1)
--                 ,(x < 0     , -1)
--                 ,(otherwise ,  0)]

-- isVar
isVar (Var x)  = True
isVar (GVar i) = True
isVar _        = False

varName (Var x) = x
varName (GVar i) = "_G" ++ show i

-- isJust
isJust (Just _) = True
isJust Nothing  = False

-- fromJust
fromJust (Just i) = i

-- Part 1 : Make unification engine

unify :: Pattern -> Pattern -> Bindings -> Bindings
unify _ _ Fail              = Fail

unify x@(Var v) y         b = unifyVariable x y b
unify y         x@(Var v) b = unifyVariable x y b

unify (Funct a aa) (Funct c cc) b =
  if a == c
    then unifyList aa cc b
    else Fail

unifyList :: [Pattern] -> [Pattern] -> Bindings -> Bindings
unifyList [] [] bindings = bindings
unifyList (x:xx) (y:yy) bindings = unifyList xx yy $ unify x y bindings
unifyList [] _ _ = Fail
unifyList _ [] _ = Fail

unifyVariable :: Pattern -> Pattern -> Bindings -> Bindings
unifyVariable x@(Var xx) y@(Var yy) bindings =
  if hasBinding xx bindings
    then unify y (lookupBindings xx bindings) bindings
    else if hasBinding yy bindings
      then unify x (lookupBindings yy bindings) bindings
      else extendBindings xx y $ extendBindings yy x bindings

unifyVariable x@(Var v) y bindings =
  if hasBinding v bindings
    then let j = lookupBindings v bindings
      in case j of
        (Var a) -> unify j y $ extendBindings v y bindings
        _       ->  if j == y
                      then bindings
                      else unify j y bindings
    else extendBindings v y bindings

-- Part 2 : Rename variables


proveAll :: [Pattern] -> Database -> Bindings -> [Bindings]

proveAll [] defs b = [b]
proveAll p defs b =
  foldl (\x y -> concatMap (\z -> prove y defs z) x) [b] p

prove :: Pattern -> Database -> Bindings -> [Bindings]

prove p@(Funct f v) defs b =
  concatMap (\a -> proveClause p defs a b) $ lookupClause f defs 


proveClause :: Pattern -> Database -> [Pattern] -> Bindings -> [Bindings]
proveClause p@(Funct f v) defs clause b = 
  case unify p (head clause) b of 
    Fail  -> [Fail]
    a     -> proveAll (tail clause) defs a 

-- Print relevant bindings

varsIn [] hs = hs
varsIn (v@(Var i) : cs) hs = varsIn cs (S.insert (show v) hs)
varsIn (v@(GVar i) : cs) hs = varsIn cs (S.insert (show v) hs)
varsIn (Funct f xx : cs) hs = varsIn (xx ++ cs) hs
varsIn (_ : cs) hs = varsIn cs hs

showBindings query NoBindings = "Yes."
showBindings query Fail = ""
showBindings query (Bindings bindings) =
   S.foldr (\ v st -> v ++ " = " ++ (show $ fromJust $ H.lookup v bindings) ++ "\n" ++ st) "" (varsIn query S.empty)

repl = replAux H.empty

replAux defs =
  do putStr "PL> "
     l <- getLine
     case parse parseProlog "Expression" l of
       Right exp -> case exp of
         Query q             -> do putStrLn "You entered a query."
                                   let res = proveAll q defs NoBindings
                                   putStrLn $ concatMap (showBindings q) res
                                   replAux defs
         Implies (Obj "bye") _ -> putStrLn "Bye."
         Implies (Obj "listing") _   -> do putStrLn $ show defs
                                           replAux defs
         Implies (Obj p)     _       -> do putStrLn $ show p
                                           replAux defs
         Implies f@(Funct p args) xx
             -> do putStrLn $ (show f) ++ " :- " ++
                       (concat (L.intersperse ", " (map show xx)))
                   putStrLn "Noted."
                   replAux $ addClause (f:xx) defs
       Left pe   -> do putStr (show pe)
                       replAux defs