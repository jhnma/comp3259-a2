module Interp where

import Declare
import Prelude hiding (LT, GT, EQ)
import Data.Maybe (fromJust)

unary :: UnaryOp -> Value -> Value
unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i)  = IntV (-i)

binary :: BinaryOp -> Value -> Value -> Value
binary Add (IntV a) (IntV b) = IntV (a + b)
binary Sub (IntV a) (IntV b) = IntV (a - b)
binary Mult (IntV a) (IntV b) = IntV (a * b)
binary Div (IntV a) (IntV b) = IntV (a `div` b)
binary And (BoolV a) (BoolV b) = BoolV (a && b)
binary Or (BoolV a) (BoolV b) = BoolV (a || b)
binary LT (IntV a) (IntV b) = BoolV (a < b)
binary LE (IntV a) (IntV b) = BoolV (a <= b)
binary GE (IntV a) (IntV b) = BoolV (a >= b)
binary GT (IntV a) (IntV b) = BoolV (a > b)
binary EQ a b = BoolV (a == b)

type Binding = (String, Value)
type Env = [Binding]

findFunction :: String -> FunEnv -> Maybe Function
findFunction _ []         = Nothing
findFunction fname (x:xs) = if (fst x == fname) then (Just (snd x)) else findFunction fname xs

execute :: Program -> Value
execute (Program funEnv main) = evaluate main [] funEnv

evaluate :: Exp -> Env -> FunEnv -> Value
evaluate e env fenv = eval e env
  where
    eval :: Exp -> Env -> Value
    eval (Call fun args) env = case findFunction fun fenv of
      Just (Function params exp) -> eval exp (env ++ (map (\x -> (fst (snd x), eval (fst x) env)) (zip args params)))
      Nothing                    -> error (fun ++ " is undefined as a function")
    eval (Lit n) _ = n
    eval (Unary op ex) env = unary op (eval ex env)
    eval (Bin op e1 e2) env = binary op (eval e1 env) (eval e2 env)
    eval (If e1 e2 e3) env =
      let BoolV test = eval e1 env
      in if test
         then eval e2 env
         else eval e3 env
    eval (Var v) env = fromJust (lookup v env)
    eval (Decl v a b) env =
      let a' = eval a env
          env' = (v, a') : env
      in eval b env'

evaluate' :: Exp -> Env -> Value
evaluate' (Lit n) env = n
evaluate' (Unary op e) env = unary op (evaluate' e env)
evaluate' (Bin op e1 e2) env = binary op (evaluate' e1 env) (evaluate' e2 env)
evaluate' (If e1 e2 e3) env =
  let BoolV test = evaluate' e1 env
  in if test
     then evaluate' e2 env
     else evaluate' e3 env
evaluate' (Var v) env = fromJust (lookup v env)
evaluate' (Decl v a b) env =
  let a' = evaluate' a env
      env' = (v, a') : env
  in evaluate' b env'
evaluate' _ _ = error "You are in trouble"

fsubst :: (String, Function) -> Exp -> Exp
fsubst (f, Function params body) e = case e of
  Call fname args -> if f == fname then case args of
    [] -> body
    (a : as) -> Decl (fst $ head params) a (fsubst (f, Function (tail params) body) e)
    else e
  _ -> e

repeatfsubst :: FunEnv -> Exp -> Exp
repeatfsubst []     exp = exp
repeatfsubst (f:fs) exp = if result == exp then repeatfsubst fs exp else result where result = fsubst f exp

execute' :: Program -> Value
execute' (Program funEnv main) = case main of
  Lit n -> n
  Unary uop exp -> evaluate' (Unary uop (repeatfsubst funEnv exp)) []
  Bin bop exp1 exp2 -> evaluate' (Bin bop (repeatfsubst funEnv exp1) (repeatfsubst funEnv exp2)) []
  If exp1 exp2 exp3 -> evaluate' (If (repeatfsubst funEnv exp1) (repeatfsubst funEnv exp2) (repeatfsubst funEnv exp3)) []
  Var s -> evaluate' (Var s) []
  Decl str exp1 exp2 -> evaluate' (Decl str (repeatfsubst funEnv exp1) (repeatfsubst funEnv exp2)) []
  _ -> error "should not be here"
