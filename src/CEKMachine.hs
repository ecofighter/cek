{-# LANGUAGE StrictData #-}
module CEKMachine where

import Data.ByteString.Char8 (ByteString)
import Syntax

type Env = [(ByteString, Value)]

data Value
  = VInt Integer
  | VBool Bool
  | VClos Exp Env
  deriving (Eq, Show)

data Cont
  = FunK [Exp] Env Cont
  | ArgK Value [Exp] [Value] Env Cont
  | LetK ByteString Exp Env Cont
  | IfK Exp Exp Env Cont
  | HaltK
  deriving (Eq, Show)

data State
  = Run Exp Env Cont
  | Done Value
  deriving (Eq, Show)

eval :: Exp -> Maybe Value
eval expr = loop (Run expr [] HaltK)
  where
    loop :: State -> Maybe Value
    loop (Done v) = Just v
    loop (Run c e k) = loop =<< step (Run c e k)

step :: State -> Maybe State
step (Run (Int n) _ k) = applyK k (VInt n)
step (Run (Bool b) _ k) = applyK k (VBool b)
step (Run (Var x) env k) = applyK k =<< lookup x env
step (Run (Fun params body) env k) =
  applyK k (VClos (Fun params body) env)
step (Run (App f args) env k) =
  Just $ Run f env (FunK args env k)
step (Run (Let x _ e1 e2) env k) =
  Just $ Run e1 env (LetK x e2 env k)
step (Run (If cond e1 e2) env k) =
  Just $ Run cond env (IfK e1 e2 env k)
step (Done v) = Just $ Done v

-- | 継続を適用する関数
applyK :: Cont -> Value -> Maybe State
applyK (FunK [] _ _) _ = Nothing
applyK (FunK (arg : args) env k) v =
  Just $ Run arg env (ArgK v args [] env k)
applyK (ArgK f [] vals _ k) v =
  case f of
    VClos (Fun params body) cenv ->
      let newEnv = zip (map fst params) (reverse (v : vals)) ++ cenv
       in Just $ Run body newEnv k
    _ -> Nothing
applyK (ArgK f (arg : args) vals env k) v =
  Just $ Run arg env (ArgK f args (v : vals) env k)
applyK (LetK x e2 env k) v =
  Just $ Run e2 ((x, v) : env) k
applyK (IfK e1 e2 env k) v =
  case v of
    VBool True -> Just $ Run e1 env k
    VBool False -> Just $ Run e2 env k
    _ -> Nothing
applyK HaltK v = Just $ Done v
