module Declare where

import Data.Maybe (fromJust)
import Prelude hiding (LT, GT, EQ)

data BinaryOp = Add | Sub | Mult | Div
              | And | Or  | GT   | LT  | LE
              | GE  | EQ
              deriving Eq

data UnaryOp
  = Neg
  | Not

data Value
  = IntV Int    -- Integers
  | BoolV Bool  -- Boolean values
  deriving Eq

data Type
  = TInt
  | TBool
  deriving Eq

instance Show Value where
  show (IntV n) = show n
  show (BoolV True) = "true"
  show (BoolV False) = "false"

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"



data Program = Program FunEnv Exp

type FunEnv = [(String, Function)]

data Function = Function [(String, Type)] Exp

data Exp = Lit Value
         | Unary UnaryOp Exp
         | Bin BinaryOp Exp Exp
         | If Exp Exp Exp
         | Var String
         | Decl String Exp Exp
         | Call String [Exp]

prog1 :: Program
prog1 =
  Program
    [ ("absolute", Function [("x", TInt)]
                     (If (Bin GT (Var "x") (Lit (IntV 0))) (Var "x") (Unary Neg (Var "x"))))
    , ("max", Function [("x", TInt), ("y", TInt)]
                (If (Bin GT (Var "x") (Var "y")) (Var "x") (Var "y")))
    ]
    (Call "max" [Call "absolute" [Lit (IntV (-5))], Lit (IntV 4)])




-- Pretty printer

-- | Examples:
--
-- >>> show prog1
-- "function absolute(x: Int) {\n  if (x > 0) x; else -x\n}\nfunction max(x: Int, y: Int) {\n  if (x > y) x; else y\n}\nmax(absolute(-5), 4)"
instance Show Program where
  show (Program fenv e) = showSep "\n" (map showFun fenv) ++ "\n" ++ show e

-- | Examples:
--
-- >>> showSep "; " ["hello", "dear", "world"]
-- "hello; dear; world"
--
-- >>> showSep "; " ["hello"]
-- "hello"
--
-- showSep "; " []
-- ""
showSep :: String -> [String] -> String
showSep sep []     = []
showSep sep [x]    = x
showSep sep (x:xs) = x ++ sep ++ showSep sep xs

-- | Examples:
--
-- >>> showFun ("foo", Function [("x",TInt), ("y",TInt)] (Bin Add (Var "x") (Var "y")))
-- "function foo(x: Int, y: Int) {\n  x + y\n}"
showFun :: (String, Function) -> String
showFun (f, (Function args exp)) = "function " ++ f ++ "(" ++ showSep ", " (map (\x -> fst x ++ ": " ++ show (snd x)) args) ++ ") " ++ "{\n  " ++ show exp ++ "\n}"

-- | Examples:
--
-- >>> show (Call "max" [(Lit (IntV 3)), (Lit (IntV 4))])
-- "max(3, 4)"
--
-- >>> show (Call "abs" [(Lit (IntV 3))])
-- "abs(3)"
instance Show Exp where
  show = showExp 0

showExp :: Int -> Exp -> String
showExp _ (Call f args) = f ++ "(" ++ showSep ", " (map show args) ++ ")"
showExp _ (Lit i) = show i
showExp _ (Var x) = x
showExp level (Decl x a b) =
  if level > 0
    then paren result
    else result
  where
    result = "var " ++ x ++ " = " ++ showExp 0 a ++ "; " ++ showExp 0 b
showExp level (If a b c) =
  if level > 0
    then paren result
    else result
  where
    result = "if (" ++ showExp 0 a ++ ") " ++ showExp 0 b ++ "; else " ++ showExp 0 c
showExp _ (Unary Neg a) = "-" ++ showExp 99 a
showExp _ (Unary Not a) = "!" ++ showExp 99 a
showExp level (Bin op a b) =
  showBin level (fromJust (lookup op levels)) a (fromJust (lookup op names)) b
  where
    levels =
      [ (Or, 1)
      , (And, 2)
      , (GT, 3)
      , (LT, 3)
      , (LE, 3)
      , (GE, 3)
      , (EQ, 3)
      , (Add, 4)
      , (Sub, 4)
      , (Mult, 5)
      , (Div, 5)
      ]
    names =
      [ (Or, "||")
      , (And, "&&")
      , (GT, ">")
      , (LT, "<")
      , (LE, "<=")
      , (GE, ">=")
      , (EQ, "==")
      , (Add, "+")
      , (Sub, "-")
      , (Mult, "*")
      , (Div, "/")
      ]

showBin :: Int -> Int -> Exp -> String -> Exp -> String
showBin outer inner a op b =
  if inner < outer
    then paren result
    else result
  where
    result = showExp inner a ++ " " ++ op ++ " " ++ showExp inner b

paren :: String -> String
paren x = "(" ++ x ++ ")"
