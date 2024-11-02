{-# LANGUAGE MonoLocalBinds #-}

module TypeChecker where

import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Syntax

-- Type environment
type TypeEnv = Map.Map ByteString Ty

newtype TcState = MkTcState {unTcState :: Int}

-- Fresh type variable generator
fresh :: (State TcState :> es) => Eff es Ty
fresh = do
  MkTcState n <- get
  put $ MkTcState (n + 1)
  return $ TyVar n

-- Substitution type and operations
type Subst = Map.Map Int Ty

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- Type substitution
class Types a where
  ftv :: a -> Set.Set Int -- Free type variables
  apply :: Subst -> a -> a -- Apply substitution

instance Types Ty where
  ftv TyInt = Set.empty
  ftv TyBool = Set.empty
  ftv (TyArr t1 t2) = ftv t1 `Set.union` ftv t2
  ftv (TyVar n) = Set.singleton n

  apply _ TyInt = TyInt
  apply _ TyBool = TyBool
  apply s (TyArr t1 t2) = TyArr (apply s t1) (apply s t2)
  apply s (TyVar n) = Map.findWithDefault (TyVar n) n s

instance Types a => Types [a] where
  ftv = foldr (Set.union . ftv) Set.empty
  apply s = map (apply s)

instance Types TypeEnv where
  ftv = ftv . Map.elems
  apply s = Map.map (apply s)

-- Unification
unify :: (Error String :> es) => Ty -> Ty -> Eff es Subst
unify TyInt TyInt = return nullSubst
unify TyBool TyBool = return nullSubst
unify (TyArr l1 r1) (TyArr l2 r2) = do
  s1 <- unify l1 l2
  s2 <- unify (apply s1 r1) (apply s1 r2)
  return $ s2 `composeSubst` s1
unify (TyVar n) t = bindVar n t
unify t (TyVar n) = bindVar n t
unify t1 t2 = throwError $ "Cannot unify " ++ show t1 ++ " with " ++ show t2

-- Binding variables
bindVar :: Error String :> es => Int -> Ty -> Eff es Subst
bindVar n t
  | t == TyVar n = return nullSubst
  | n `Set.member` ftv t = throwError "Occurs check fails"
  | otherwise = return $ Map.singleton n t

-- Type inference
infer :: (Error String :> es, State TcState :> es) => TypeEnv -> Exp -> Eff es (Subst, Ty)
infer _ (Int _) = return (nullSubst, TyInt)
infer _ (Bool _) = return (nullSubst, TyBool)
infer env (Var x) = case Map.lookup x env of
  Nothing -> throwError $ "Unbound variable: " ++ show x
  Just t -> return (nullSubst, t)
infer env (Fun params body) = do
  -- Generate fresh type variables for parameters
  paramTyVars <- mapM (\(_, mty) -> maybe fresh return mty) params
  -- Create new environment with parameter bindings
  let paramBindings = zip (map fst params) paramTyVars
      env' = foldr (uncurry Map.insert) env paramBindings
  -- Infer body type in the new environment
  (s, bodyTy) <- infer env' body
  -- Build function type backwards through the parameter list
  let finalTy = foldr (TyArr . apply s) (apply s bodyTy) paramTyVars
  return (s, finalTy)
infer env (App f args) = do
  (s1, funTy) <- infer env f
  -- Process arguments one by one, maintaining substitution chain
  (s2, argTys) <- foldM inferArg (s1, []) args
  resultTy <- fresh
  let expectedTy = foldr TyArr resultTy (reverse argTys)
  s3 <- unify (apply s2 funTy) expectedTy
  return (s3 `composeSubst` s2, apply s3 resultTy)
  where
    inferArg (s, tys) arg = do
      (s', ty) <- infer (apply s env) arg
      return (s' `composeSubst` s, ty : tys)
infer env (Let x tyAnn e1 e2) = do
  (s1, t1) <- infer env e1
  case tyAnn of
    Just t -> do
      s2 <- unify t1 t
      let s = s2 `composeSubst` s1
          env' = Map.insert x (apply s t) (apply s env)
      (s3, t2) <- infer env' e2
      return (s3 `composeSubst` s, t2)
    Nothing -> do
      let env' = Map.insert x t1 (apply s1 env)
      (s2, t2) <- infer env' e2
      return (s2 `composeSubst` s1, t2)
infer env (If cond e1 e2) = do
  (s1, condTy) <- infer env cond
  s2 <- unify condTy TyBool
  let s = s2 `composeSubst` s1
      env' = apply s env
  (s3, t1) <- infer env' e1
  (s4, t2) <- infer (apply s3 env') e2
  s5 <- unify (apply s4 t1) t2
  let finalSubst = foldr composeSubst nullSubst [s5, s4, s3, s]
  return (finalSubst, apply finalSubst t2)

-- Main type checking function
typeCheck :: Exp -> Either String Ty
typeCheck expr = do
  (subst, ty) <- runPureEff . runErrorNoCallStack $ evalState (MkTcState 0) (infer Map.empty expr)
  pure $ apply subst ty
