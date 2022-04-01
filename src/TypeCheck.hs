module TypeCheck where

import Declare
import Prelude hiding (LT, GT, EQ)

type TEnv = [(String,Type)]

type TFunEnv = [(String, (TEnv, Type))]

tunary :: UnaryOp -> Type -> Maybe Type
tunary Neg TInt = Just TInt
tunary Not TBool = Just TBool
tunary _ _ = Nothing

tbinary :: BinaryOp -> Type -> Type -> Maybe Type
tbinary Add  TInt  TInt  = Just TInt
tbinary Sub  TInt  TInt  = Just TInt
tbinary Mult TInt  TInt  = Just TInt
tbinary Div  TInt  TInt  = Just TInt
tbinary And  TBool TBool = Just TBool
tbinary Or   TBool TBool = Just TBool
tbinary LT   TInt  TInt  = Just TBool
tbinary LE   TInt  TInt  = Just TBool
tbinary GE   TInt  TInt  = Just TBool
tbinary GT   TInt  TInt  = Just TBool
tbinary EQ   t1    t2    | t1 == t2 = Just TBool
tbinary _ _ _ = Nothing


-- | Type checking function definitions
--
-- Examples:
--
-- >>> :{
--   checkFunEnv [ ("foo", Function [("x",TInt), ("y",TInt)] (Bin Add (Var "x") (Var "y")))
--               , ("bar", Function [("x",TInt)] (Call "foo" [Var "x", Var "x"]))
--               ]
-- :}
-- Just [("foo",([("x",Int),("y",Int)],Int)),("bar",([("x",Int)],Int))]
--
-- >>> :{
--   checkFunEnv [ ("foo", Function [("x",TInt), ("y",TInt)] (Bin Add (Var "x") (Var "y")))
--               , ("err", Function [("x",TInt)] (Call "foo" [Var "x"]))
--               ]
-- :}
-- Nothing
--
-- >>> checkFunEnv [ ("err", Function [("x",TInt)] (Call "foo" [Var "x", Var "x"])) ]
-- Nothing
--
-- >>> checkFunEnv [ ("err", Function [("x",TBool)] (Bin Add (Var "x") (Var "x"))) ]
-- Nothing

checkFunEnv :: FunEnv -> Maybe TFunEnv
checkFunEnv fds = checkFunEnv1 fds [] -- starts with an empty function type environment
  where
    checkFunEnv1 :: FunEnv -> TFunEnv -> Maybe TFunEnv
    checkFunEnv1 fds tfe = case fds of
      []                                    -> Just tfe
      ((fname, (Function params exp)) : fs) -> case tcheck exp params tfe of
        Just returnTy -> case checkFunEnv1 fs (tfe ++ [(fname, (params, returnTy))]) of
          Just finalTfe -> Just finalTfe
          Nothing -> Nothing
        Nothing -> Nothing          -- function doesn't type check



functionTCheck :: String -> [Exp] -> TEnv -> TFunEnv -> Bool
functionTCheck _ _ _ []  = False
functionTCheck fname args tenv fenv =
  if fname == tfname
    then (map (\x -> tcheck x tenv fenv) args) == (map (\x -> Just (snd x)) params)
    else False
  where ((tfname, (params, returnTy)) : fs) = fenv


-- When running this function, it is already checked that the function exists
findReturnType :: String -> TFunEnv -> Type
findReturnType fname []          = error "Not supposed to be here"
findReturnType fname (f : fenvs) = if fst f == fname then (snd (snd f)) else findReturnType fname fenvs


tcheck :: Exp -> TEnv -> TFunEnv -> Maybe Type
tcheck (Call name args) tenv fenv = if functionTCheck name args tenv fenv then (Just (findReturnType name fenv)) else Nothing
tcheck (Lit v) _ _ =
  case v of
    IntV _ -> Just TInt
    BoolV _ -> Just TBool
tcheck (Unary op e) tenv fenv =
  case tcheck e tenv fenv of
    Just t  -> tunary op t
    Nothing -> Nothing
tcheck (Bin op e1 e2) tenv fenv =
  case (tcheck e1 tenv fenv, tcheck e2 tenv fenv) of
    (Just t1, Just t2) -> tbinary op t1 t2
    _                  -> Nothing
tcheck (If e1 e2 e3) tenv fenv =
  case tcheck e1 tenv fenv of
    Just TBool ->
      case tcheck e2 tenv fenv of
        Just t1 ->
          if Just t1 ==
             tcheck e3 tenv fenv
            then Just t1
            else Nothing
        Nothing -> Nothing
    _ -> Nothing
tcheck (Var v) tenv _ = lookup v tenv
tcheck (Decl v e1 e2) tenv fenv =
  case tcheck e1 tenv fenv of
    Just t  -> tcheck e2 ((v, t) : tenv) fenv
    Nothing -> Nothing


-- | Type checking function definitions
--
-- Examples:
--
-- >>> checkProgram prog1
-- True
--
-- >>> checkProgram prog2
-- True
--
-- >>> checkProgram (Program [] (Call "max" [Call "absolute" [Lit (IntV (-5))], Lit (IntV 4)]))
-- False

checkProgram :: Program -> Bool
checkProgram (Program funenv exp) = case checkFunEnv funenv of
  Just tfunenv -> case tcheck exp [] tfunenv of
    Just ty -> True
    Nothing -> False
  Nothing -> False

-- Question 10: No.